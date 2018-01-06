from PIL import Image
import numpy as np
from glob import glob
import os
import re

def pad_chipsets(path='../resources/Chipset/*.png'):
    files = glob(path)
    files = [f for f in files if not f.endswith('.elm.png')]
    print('padding %d images...' % len(files))
    for f in files:
        pad_image(f)
    print('done')

def clean_data(path='../resources/*', extensions=('emu', 'emt', 'edb')):
    files = []
    for f in glob(path):
        for ext in extensions:
            if f.endswith('.' + ext) and not f.endswith('.elm.' + ext):
                files.append(f)
                continue
    print('cleaning %d files...' % len(files))
    tags = [
        ('<?xml version="1.0" encoding="UTF-8"?>', '<LMU>', '</LMU>'),
        ('<?xml version="1.0" encoding="UTF-8"?>', '<LDB>', '</LDB>'),
        ('<?xml version="1.0" encoding="UTF-8"?>', '<LMT>', '</LMT>')
    ]
    for f in files:
        removed = simplify_xml(f)
        assert(removed in tags )
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

def simplify_xml(filename):
    """Quick hack to reduce Easy_RT XML files to single tag by removing outer
    XML.
    <?xml version="1.0" encoding="UTF-8"?>
    <LMU>
     [...keep...]
    </LMU>
    """
    filename = os.path.abspath(filename)
    a, b = filename.split('.')
    filename_out = '.'.join([a, 'elm', b])

    with open(filename, 'r') as fh:
        lines = fh.read().strip().split('\n')
        new_text = '\n'.join(lines[2:-1])
        removed = lines[0], lines[1], lines[-1]

    with open(filename_out, 'w') as fh:
        fh.write(new_text)

    return removed


def parse_BlockA_subtile_IDs(raw):
    """Convert to i,j convention. Only i values provided (j used for animation).
    """
    pat = '\{([-\d]+), ([-\d]+)\}, \{([-\d]+), ([-\d]+)\}'

    arr = []
    for m in re.findall(pat, raw):
        quadrants = map(int, m)
        arr.append(quadrants)

    return arr


def parse_BlockD_subtile_IDs(raw):
    """Convert to i,j convention.
    """
    pat = '\{\{\{(\d), (\d)\}, {(\d), (\d)\}\}, \{\{(\d), (\d)\}, {(\d), (\d)\}\}\}'

    arr = []
    for m in re.findall(pat, raw):
        m = map(int, m)
        top    = (m[1], m[0]), (m[3], m[2])
        bottom = (m[5], m[4]), (m[7], m[6])
        arr.append((top, bottom))

    return arr


def load_chipset(filename):
    Image(filename)


# in tiles
BLOCK_HEIGHT = 4 
BLOCK_WIDTH  = 3

# in pixels
TILE_SIZE = 16

def find_blockAB_quadrants(index, animID=0):
    """Convert index to chipset tile coordinates of each quadrant.
    Quadrant order: top left, top right, bottom left, bottom right.

    Valid decoding range 1-100 (?).
    """
    assert(0 <= index < 100)
    assert(0 <= animID < 4)
    # calculate the 3x4 block
    # conversion from tilemap_layer.cpp, valid for larger range
    block = index / 50
    b_subtile = (index - block * 1000) / 50
    a_subtile = index - block * 1000 - b_subtile * 50

    quadrants_i = blockA_Subtiles_IDs[a_subtile]
    
    blockA_i, blockA_j = blockA_ij[block]
    # simplified (see below)
    blockB_i, blockB_j = blockB_ij[0]
    tiles_ij = []
    for quad_i in quadrants_i:
        # block B
        if quad_i == -1:
            # for simplicity, default to regular water tile
            # full solution from tilemap_layer.cpp:
            # for (int j = 0; j < 2; j++) {
            #   for (int i = 0; i < 2; i++) {
            #     int t = (b_subtile >> (j * 2 + i)) & 1;
            #     if (block == 2) t ^= 3;
            #     ... } }
            t = 0 # changing this will get water/deep water autotiles
            tile_i = blockB_i * BLOCK_HEIGHT + t
            tile_j = blockB_j * BLOCK_WIDTH  + animID

        # block A
        else:
            tile_i = blockA_i * BLOCK_HEIGHT + quad_i
            tile_j = blockA_j * BLOCK_WIDTH  + animID
        tiles_ij.append((tile_i, tile_j))

    return tiles_ij


def find_blockD_quadrants(index):
    """Convert index to chipset tile coordinates of each quadrant.
    Quadrant order: top left, top right, bottom left, bottom right.
    """
    # calculate the 3x4 block
    block = (index - 4000) / 50

    # calculate the subtile index
    # subtile => build a tile out of 4 quarter tiles
    subtile = index - 4000 - block * 50

    (topL, topR), (botL, botR) = blockD_Subtiles_IDs[subtile]
    quadrants = topL, topR, botL, botR

    block_i, block_j = blockD_ij[block]
    
    tiles_ij = []
    for quad_i, quad_j in quadrants:
        tile_i = block_i * BLOCK_HEIGHT + quad_i
        tile_j = block_j * BLOCK_WIDTH  + quad_j
        tiles_ij.append((tile_i, tile_j))

    return tiles_ij

def assemble_quadrants(chipset, tiles_ij):
    it = zip(('topL', 'topR', 'botL', 'botR'), tiles_ij)
    if chipset.ndim == 3:
        tile_shape = (TILE_SIZE, TILE_SIZE, chipset.shape[2])
    else:
        tile_shape = (TILE_SIZE, TILE_SIZE)
    tile = np.zeros(tile_shape, dtype=chipset.dtype)

    s = TILE_SIZE / 2
    for quad, (tile_i, tile_j) in it:
        quad_offset_i = s if 'bot' in quad else 0
        quad_offset_j = s if 'R'   in quad else 0

        chipset_i = tile_i * TILE_SIZE + quad_offset_i
        chipset_j = tile_j * TILE_SIZE + quad_offset_j

        slic = lambda i, j: (slice(i, i + s), slice(j, j + s))
        tile[slic(quad_offset_i, quad_offset_j)] = chipset[slic(chipset_i, chipset_j)]

    return tile

blockA_ij = [(0, 0), (0, 1)]

# left block is water/deep water autotile
# right block is waterfall/vortex animations
blockB_ij = [(1, 0), (1, 1)]

blockD_ij = [ # first column
             (2, 0), (2, 1), 
             (3, 0), (3, 1), 
              # second column
             (0, 2), (0, 3),
             (1, 2), (1, 3),
             (2, 2), (2, 3),
             (3, 2), (3, 3)
             ]

# from EasyRPG Player tilemap_layer.cpp
blockA_Subtiles_IDs = parse_BlockA_subtile_IDs(
    """
    // Blocks subtiles IDs
    // Mess with this code and you will die in 3 days...
    // [tile-id][row][col]
    static const int8_t BlockA_Subtiles_IDS[47][2][2] = {
    #define N -1
        {{N, N}, {N, N}},
        {{3, N}, {N, N}},
        {{N, 3}, {N, N}},
        {{3, 3}, {N, N}},
        {{N, N}, {N, 3}},
        {{3, N}, {N, 3}},
        {{N, 3}, {N, 3}},
        {{3, 3}, {N, 3}},
        {{N, N}, {3, N}},
        {{3, N}, {3, N}},
        {{N, 3}, {3, N}},
        {{3, 3}, {3, N}},
        {{N, N}, {3, 3}},
        {{3, N}, {3, 3}},
        {{N, 3}, {3, 3}},
        {{3, 3}, {3, 3}},
        {{1, N}, {1, N}},
        {{1, 3}, {1, N}},
        {{1, N}, {1, 3}},
        {{1, 3}, {1, 3}},
        {{2, 2}, {N, N}},
        {{2, 2}, {N, 3}},
        {{2, 2}, {3, N}},
        {{2, 2}, {3, 3}},
        {{N, 1}, {N, 1}},
        {{N, 1}, {3, 1}},
        {{3, 1}, {N, 1}},
        {{3, 1}, {3, 1}},
        {{N, N}, {2, 2}},
        {{3, N}, {2, 2}},
        {{N, 3}, {2, 2}},
        {{3, 3}, {2, 2}},
        {{1, 1}, {1, 1}},
        {{2, 2}, {2, 2}},
        {{0, 2}, {1, N}},
        {{0, 2}, {1, 3}},
        {{2, 0}, {N, 1}},
        {{2, 0}, {3, 1}},
        {{N, 1}, {2, 0}},
        {{3, 1}, {2, 0}},
        {{1, N}, {0, 2}},
        {{1, 3}, {0, 2}},
        {{0, 0}, {1, 1}},
        {{0, 2}, {0, 2}},
        {{1, 1}, {0, 0}},
        {{2, 0}, {2, 0}},
        {{0, 0}, {0, 0}}
    #undef N
    };
    """.replace('N', '-1')
    )

blockD_Subtiles_IDs = parse_BlockD_subtile_IDs(
    """
    // [tile-id][row][col][x/y]
    static const uint8_t BlockD_Subtiles_IDS[50][2][2][2] = {
    //     T-L     T-R       B-L     B-R
        {{{1, 2}, {1, 2}}, {{1, 2}, {1, 2}}},
        {{{2, 0}, {1, 2}}, {{1, 2}, {1, 2}}},
        {{{1, 2}, {2, 0}}, {{1, 2}, {1, 2}}},
        {{{2, 0}, {2, 0}}, {{1, 2}, {1, 2}}},
        {{{1, 2}, {1, 2}}, {{1, 2}, {2, 0}}},
        {{{2, 0}, {1, 2}}, {{1, 2}, {2, 0}}},
        {{{1, 2}, {2, 0}}, {{1, 2}, {2, 0}}},
        {{{2, 0}, {2, 0}}, {{1, 2}, {2, 0}}},
        {{{1, 2}, {1, 2}}, {{2, 0}, {1, 2}}},
        {{{2, 0}, {1, 2}}, {{2, 0}, {1, 2}}},
        {{{1, 2}, {2, 0}}, {{2, 0}, {1, 2}}},
        {{{2, 0}, {2, 0}}, {{2, 0}, {1, 2}}},
        {{{1, 2}, {1, 2}}, {{2, 0}, {2, 0}}},
        {{{2, 0}, {1, 2}}, {{2, 0}, {2, 0}}},
        {{{1, 2}, {2, 0}}, {{2, 0}, {2, 0}}},
        {{{2, 0}, {2, 0}}, {{2, 0}, {2, 0}}},
        {{{0, 2}, {0, 2}}, {{0, 2}, {0, 2}}},
        {{{0, 2}, {2, 0}}, {{0, 2}, {0, 2}}},
        {{{0, 2}, {0, 2}}, {{0, 2}, {2, 0}}},
        {{{0, 2}, {2, 0}}, {{0, 2}, {2, 0}}},
        {{{1, 1}, {1, 1}}, {{1, 1}, {1, 1}}},
        {{{1, 1}, {1, 1}}, {{1, 1}, {2, 0}}},
        {{{1, 1}, {1, 1}}, {{2, 0}, {1, 1}}},
        {{{1, 1}, {1, 1}}, {{2, 0}, {2, 0}}},
        {{{2, 2}, {2, 2}}, {{2, 2}, {2, 2}}},
        {{{2, 2}, {2, 2}}, {{2, 0}, {2, 2}}},
        {{{2, 0}, {2, 2}}, {{2, 2}, {2, 2}}},
        {{{2, 0}, {2, 2}}, {{2, 0}, {2, 2}}},
        {{{1, 3}, {1, 3}}, {{1, 3}, {1, 3}}},
        {{{2, 0}, {1, 3}}, {{1, 3}, {1, 3}}},
        {{{1, 3}, {2, 0}}, {{1, 3}, {1, 3}}},
        {{{2, 0}, {2, 0}}, {{1, 3}, {1, 3}}},
        {{{0, 2}, {2, 2}}, {{0, 2}, {2, 2}}},
        {{{1, 1}, {1, 1}}, {{1, 3}, {1, 3}}},
        {{{0, 1}, {0, 1}}, {{0, 1}, {0, 1}}},
        {{{0, 1}, {0, 1}}, {{0, 1}, {2, 0}}},
        {{{2, 1}, {2, 1}}, {{2, 1}, {2, 1}}},
        {{{2, 1}, {2, 1}}, {{2, 0}, {2, 1}}},
        {{{2, 3}, {2, 3}}, {{2, 3}, {2, 3}}},
        {{{2, 0}, {2, 3}}, {{2, 3}, {2, 3}}},
        {{{0, 3}, {0, 3}}, {{0, 3}, {0, 3}}},
        {{{0, 3}, {2, 0}}, {{0, 3}, {0, 3}}},
        {{{0, 1}, {2, 1}}, {{0, 1}, {2, 1}}},
        {{{0, 1}, {0, 1}}, {{0, 3}, {0, 3}}},
        {{{0, 3}, {2, 3}}, {{0, 3}, {2, 3}}},
        {{{2, 1}, {2, 1}}, {{2, 3}, {2, 3}}},
        {{{0, 1}, {2, 1}}, {{0, 3}, {2, 3}}},
        {{{1, 2}, {1, 2}}, {{1, 2}, {1, 2}}},
        {{{1, 2}, {1, 2}}, {{1, 2}, {1, 2}}},
        {{{0, 0}, {0, 0}}, {{0, 0}, {0, 0}}}
    };
    """
    )


def montage(arr, shape=None):
    """tile ND arrays ([..., height, width]) in last two dimensions
    first N-2 dimensions must match, tiles are expanded to max height and width
    pads with zero, no spacing
    if shape=(rows, columns) not provided, defaults to square, clipping last row if empty
    """
    from itertools import product

    sz = zip(*[img.shape for img in arr])
    h, w, n = max(sz[-2]), max(sz[-1]), len(arr)
    if not shape:
        nr = nc = int(np.ceil(np.sqrt(n)))
        if (nr - 1) * nc >= n:
            nr -= 1
    else:
        nr, nc = shape
    M = np.zeros(arr[0].shape[:-2] + (nr * h, nc * w), dtype=arr[0].dtype)

    for (r, c), img in zip(product(range(nr), range(nc)), arr):
        s = [[None] for _ in img.shape]
        s[-2] = (r * h, r * h + img.shape[-2])
        s[-1] = (c * w, c * w + img.shape[-1])
        M[[slice(*x) for x in s]] = img

    return M

def run():
    pad_chipsets()
    clean_data()
