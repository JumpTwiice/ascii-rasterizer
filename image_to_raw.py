from PIL import Image
import struct
import sys

def image_to_raw(image_path, output_path):
    img = Image.open(image_path).convert("RGBA")
    width, height = img.size
    pixel_data = img.tobytes()
    raw_data = bytearray()
    raw_data += struct.pack('<II', width, height)
    raw_data += pixel_data 

    with open(output_path, 'wb') as f:
        f.write(raw_data)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python image_to_raw.py <input_image> <output_raw>")
        sys.exit(1)

    input_image = sys.argv[1]
    output_raw = sys.argv[2]
    image_to_raw(input_image, output_raw)
    print(f"Converted {input_image} to {output_raw}")
