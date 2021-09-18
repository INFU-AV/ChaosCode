columns = 16
order_from_bottom = True


# This needs the PIL library. install with "pip3 install PIL"
import sys
from PIL import Image


# Open all the images specified in the command line parameters and put them into an array
images = [Image.open(x) for x in sys.argv[1:]]
# Create two arrays that have the height and width of each image
widths, heights = zip(*(i.size for i in images))

# Create an array of the width and height of each row
row_widths=[]
row_heights=[]
tempheight=0
tempwidth=0
for i in range(0,len(images)):
  tempwidth+=images[i].size[0]
  tempheight=max([images[i].size[1],tempheight])
  if i % columns == 0 and i != 0:
    row_heights.append(tempheight)
    row_widths.append(tempwidth)
    tempheight=0
    tempwidth=0

if tempheight !=0:
  row_heights.append(tempheight)
  row_widths.append(tempwidth)

# Calculate the maximum width and total height
max_width = max(row_widths)
total_height = sum(row_heights)

# Create a new image with the width of the longest row, and the height of all rows combined
new_im = Image.new('RGBA', (max_width, total_height))

# For each image: Paste image in the newly created image next to the last image that got pasted
row=0
x_offset = 0
y_offset = 0

for i in range(0,len(images)):
  if order_from_bottom:
    new_im.paste(images[i], (x_offset,total_height-y_offset-row_heights[row]))
  else:
    new_im.paste(images[i], (x_offset,y_offset))
  x_offset += images[i].size[0]
  if i % columns == 0 and i !=0:
    x_offset=0
    y_offset+=row_heights[row]
    row+=1

# Save that image to "output.png"
new_im.save('output.png')
