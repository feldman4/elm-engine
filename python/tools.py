from PIL import Image
from glob import glob

def pad_chipsets(path='../resources/Chipset/*.png'):
    files = glob(path)
    files = [f for f in files if not f.endswith('.elm.png')]
    print('padding %d images...' % len(files))
    for f in files:
        pad_image(f)
    print('done')

def pad_image(filename, size=(512, 256), background=(0, 128, 128)):
    """Pad image for use as GL texture.
    """
    filename_out = filename.replace('.png', '.elm.png')
    img = Image.open(filename)

    img_pad = Image.new('RGB', size, background)
    img_pad.paste(img, img.getbbox())
    img_pad.save(filename_out)

    return img_pad
