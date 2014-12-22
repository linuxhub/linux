/*
 * FB driver for the ILI9486 LCD Controller
 *
 * Copyright (C) 2014 Noralf Tronnes
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/uaccess.h>
#include <linux/errno.h>

#include "fbtft.h"

#define DRVNAME		"fb_ili9486_3"
#define WIDTH		320
#define HEIGHT		480


/* this init sequence matches PiScreen */
static int default_init_sequence_9486[] = {
	/* Interface Mode Control */
	-1, 0xb0, 0x0,
	/* Sleep OUT */
	-1, 0x11,
	-2, 250,
	/* Interface Pixel Format */
	-1, 0x3A, 0x66,
	/* Power Control 3 */
	-1, 0xC2, 0x44,
	/* VCOM Control 1 */
	-1, 0xC5, 0x00, 0x00, 0x00, 0x00,
	/* PGAMCTRL(Positive Gamma Control) */
	-1, 0xE0, 0x0F, 0x1F, 0x1C, 0x0C, 0x0F, 0x08, 0x48, 0x98,
	          0x37, 0x0A, 0x13, 0x04, 0x11, 0x0D, 0x00,
	/* NGAMCTRL(Negative Gamma Control) */
	-1, 0xE1, 0x0F, 0x32, 0x2E, 0x0B, 0x0D, 0x05, 0x47, 0x75,
	          0x37, 0x06, 0x10, 0x03, 0x24, 0x20, 0x00,
	/* Digital Gamma Control 1 */
	-1, 0xE2, 0x0F, 0x32, 0x2E, 0x0B, 0x0D, 0x05, 0x47, 0x75,
	          0x37, 0x06, 0x10, 0x03, 0x24, 0x20, 0x00,
	/* Sleep OUT */
	-1, 0x11,
	/* Display ON */
	-1, 0x29,
	/* end marker */
	-3
};

static int default_init_sequence_9481[] = {

	/* SLP_OUT - Sleep out */
	-1, 0x11,
	-2, 50,
	/* Power setting */
	-1, 0xD0, 0x07, 0x42, 0x18,
	/* VCOM */
	-1, 0xD1, 0x00, 0x07, 0x10,
	/* Power setting for norm. mode */
	-1, 0xD2, 0x01, 0x02,
	/* Panel driving setting */
	-1, 0xC0, 0x10, 0x3B, 0x00, 0x02, 0x11,
	/* Frame rate & inv. */
	-1, 0xC5, 0x03,
	/* Pixel format */
	-1, 0x3A, 0x55,
	/* Gamma */
	-1, 0xC8, 0x00, 0x32, 0x36, 0x45, 0x06, 0x16,
		  0x37, 0x75, 0x77, 0x54, 0x0C, 0x00,
	/* DISP_ON */
	-1, 0x29,
	-3
};

static void set_addr_win(struct fbtft_par *par, int xs, int ys, int xe, int ye)
{
	fbtft_par_dbg(DEBUG_SET_ADDR_WIN, par,
		"%s(xs=%d, ys=%d, xe=%d, ye=%d)\n", __func__, xs, ys, xe, ye);

	/* Column address */
	write_reg(par, 0x2A, xs >> 8, xs & 0xFF, xe >> 8, xe & 0xFF);

	/* Row adress */
	write_reg(par, 0x2B, ys >> 8, ys & 0xFF, ye >> 8, ye & 0xFF);

	/* Memory write */
	write_reg(par, 0x2C);
}

static int set_var(struct fbtft_par *par)
{
	fbtft_par_dbg(DEBUG_INIT_DISPLAY, par, "%s()\n", __func__);

	switch (par->info->var.rotate) {
	case 0:
		write_reg(par, 0x36, 0x80 | (par->bgr << 3));
		break;
	case 90:
		write_reg(par, 0x36, 0x20 | (par->bgr << 3));
		break;
	case 180:
		write_reg(par, 0x36, 0x40 | (par->bgr << 3));
		break;
	case 270:
		write_reg(par, 0x36, 0xE0 | (par->bgr << 3));
		break;
	default:
		break;
	}

	return 0;
}

void fbtft_write_reg16_bus9(struct fbtft_par *par, int len, ...)
{
	va_list args;
	int i, ret;
	int pad = 0;
	u16 *buf = (u16 *)par->buf;

	if (unlikely(par->debug & DEBUG_WRITE_REGISTER)) {
		va_start(args, len);
		for (i = 0; i < len*2; i+=2)
		{
			// fill with 0x00
			*(((u8 *)buf) + i)   = 0x00;
			*(((u8 *)buf) + i+1) = (u8)va_arg(args, unsigned int);
		}
		va_end(args);
		fbtft_par_dbg_hex(DEBUG_WRITE_REGISTER, par,
			par->info->device, u8, buf, len*2, "%s: ", __func__);
	}
	if (len <= 0)
		return;

	if (par->spi && (par->spi->bits_per_word == 8)) {
		return ;
	}

	// the cmd byte
	va_start(args, len);
	*buf++ = 0x0000;
	*buf++ = cpu_to_be16(((u8)va_arg(args, unsigned int)));

	// the parameter bytes
	i = len - 1;
	while (i--) {
		// fill with 0x00
		*buf++ = cpu_to_be16(0x0100); /* dc=1 */
		*buf++ = cpu_to_be16(((u8)va_arg(args, unsigned int)) | 0x0100);
	}
	va_end(args);

	ret = par->fbtftops.write(par, par->buf, len*2 * sizeof(u16));
	if (ret < 0) {
		dev_err(par->info->device,
			"%s: write() failed and returned %d\n", __func__, ret);
		return;
	}
}

/* 18 bit pixel (in 3 bytes) over 9-bit SPI bus: dc + high byte, dc + low byte */
int fbtft_write_vmem18_bus9(struct fbtft_par *par, size_t offset, size_t len)
{
	u8 *vmem8;
	u8 *tmp_buf=NULL;
	u16 *txbuf16 = par->txbuf.buf;
	size_t remain;
	size_t to_copy;
	size_t tx_array_size;
	int i;
	int ret = 0;

	fbtft_par_dbg(DEBUG_WRITE_VMEM, par, "%s(offset=%zu, len=%zu)\n",
		__func__, offset, len);

	if (!par->txbuf.buf) {
		dev_err(par->info->device, "%s: txbuf.buf is NULL\n", __func__);
		return -1;
	}

	remain = len;
	vmem8 = par->info->screen_base + offset;

	tx_array_size = par->txbuf.len / 2;

	while (remain) {
		to_copy = remain > tx_array_size ? tx_array_size : remain;
		dev_dbg(par->info->device, "    to_copy=%zu, remain=%zu\n",
						to_copy, remain - to_copy);

#ifdef __LITTLE_ENDIAN
		for (i = 0; i < to_copy; i += 2) {
			txbuf16[i]   = 0x0100 | vmem8[i+1];
			txbuf16[i+1] = 0x0100 | vmem8[i];
		}
#else
		for (i = 0; i < to_copy; i++)
			txbuf16[i]   = 0x0100 | vmem8[i];
#endif
		vmem8 = vmem8 + to_copy;
		// ret = par->fbtftops.write(par, par->txbuf.buf, to_copy*2);
		tmp_buf = kmalloc(to_copy*3,GFP_KERNEL);
		if(!tmp_buf)
		{
			printk("kmalloc(to_copy*3,GFP_KERNEL) err!\n");
			return -EINVAL;
		}
		for(i=0;i<to_copy;i++)
		{
			tmp_buf[3*i]   = (u8)(((txbuf16[i]>>11)&0x001f))<<2;
			tmp_buf[3*i+1] = (u8)(((txbuf16[i]>>5 )&0x003f))<<2;
			tmp_buf[3*i+2] = (u8)(((txbuf16[i]>>0 )&0x001f))<<2;
		}
		ret = par->fbtftops.write(par, tmp_buf, to_copy*3)/3*2;
		kfree(tmp_buf);

		if (ret < 0)
			return ret;
		remain -= to_copy;
	}

	return ret;
}

int fbtft_verify_gpios_no_dc(struct fbtft_par *par)
{
	struct fbtft_platform_data *pdata;
	int i;

	fbtft_par_dbg(DEBUG_VERIFY_GPIOS, par, "%s()\n", __func__);

	pdata = par->info->device->platform_data;
	if (pdata->display.buswidth != 9 && par->startbyte == 0 && \
							par->gpio.dc < 0) {
		dev_err(par->info->device,
			"Missing info about 'dc' gpio. Aborting.\n");
		return -EINVAL;
	}

	if (!par->pdev)
		return 0;

	if (par->gpio.wr < 0) {
		dev_err(par->info->device, "Missing 'wr' gpio. Aborting.\n");
		return -EINVAL;
	}
	for (i = 0; i < pdata->display.buswidth; i++) {
		if (par->gpio.db[i] < 0) {
			dev_err(par->info->device,
				"Missing 'db%02d' gpio. Aborting.\n", i);
			return -EINVAL;
		}
	}

	return 0;
}

static struct fbtft_display display = {
	.regwidth = 16,
	.width = WIDTH,
	.height = HEIGHT,
	.init_sequence = default_init_sequence_9481,
	.fbtftops = {
		.set_addr_win   = set_addr_win,
		.set_var        = set_var,
		.write_register = fbtft_write_reg16_bus9,
		.write_vmem     = fbtft_write_vmem18_bus9,
		// .verify_gpios = fbtft_verify_gpios_no_dc,
	},
};
FBTFT_REGISTER_DRIVER(DRVNAME, "ilitek,ili9486", &display);

MODULE_ALIAS("spi:" DRVNAME);
MODULE_ALIAS("platform:" DRVNAME);
MODULE_ALIAS("spi:ili9486_3");
MODULE_ALIAS("platform:ili9486_3");

MODULE_DESCRIPTION("FB driver for the ILI9486 (convert 565 to 666) LCD Controller");
MODULE_AUTHOR("Noralf Tronnes");
MODULE_LICENSE("GPL");
