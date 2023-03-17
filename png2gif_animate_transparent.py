#! /usr/bin/env python3

from PIL import Image
import os
import argparse

parser = argparse.ArgumentParser(description = "Convert all png files to a gif animation, reserve the transparent attribute.")

parser.add_argument("--duration", type = float, default = 0.1, help = "Duration of each frame, in unit s, should be a multiple of 0.01")
parser.add_argument("--reverse", type = bool, default = False, help = "Whether reverse the animation or not")
parser.add_argument("--loop", type = int, default = 0, help = "Loop how many times, 0 for infinite")
parser.add_argument("--output", type = str, default = "out.gif", help = "Output file name")

args = parser.parse_args()

def gen_frame(path):
    im = Image.open(path)
    alpha = im.getchannel('A')

    # Convert the image into P mode but only use 0xFF colors in the palette out of 0x100
    im = im.convert('RGB').convert('P', palette = Image.ADAPTIVE, colors = 0xFF)

    # Set all pixel values below 128 to 0xFF , and the rest to 0
    mask = Image.eval(alpha, lambda _: 0xFF if _ <= 0x80 else 0x00)

    # Paste the color of index 0xFF and use alpha as a mask
    im.paste(0xFF, mask)

    # The transparency index is 0xFF
    im.info['transparency'] = 0xFF

    return im

images = [gen_frame(filename) for filename in sorted([fn for fn in os.listdir(".") if fn.endswith(".png")], reverse = args.reverse)]
images[0].save(args.output if args.output.endswith(".gif") else ".".join([args.output, "gif"]), \
    save_all = True, append_images = images[1:], loop = args.loop, duration = int(1E3 * args.duration), disposal = 2) # disposal should be 2 or 3
for _ in images: _.close()

