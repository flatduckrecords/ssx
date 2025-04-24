# ssx
SAM Coupé SCREEN$ viewer for ZX Spectrum Next

I've prepared a dot command called <kbd>.SSX</kbd> for viewing SAM <kbd>MODE 3</kbd> and <kbd>MODE 4</kbd> screens using ZX Next's&nbsp;<kbd>LAYER 2</kbd>.
  
- Requires **Next core 3.0.6** or later.
- Uses Copper (co-processor) for line palette changes
- Uses Layer 2 256x192 for SAM "MODE 4"
- Uses Layer 2 in 640x256 overscan mode for SAM "MODE 3"
- Uses 9-Bit colour space
- Uses esxDOS for file access
- Uses IDEDOS/Plus3DOS APIs for memory allocation
- Uses dot-command structure and error handling for integration with NextZXOS

To install this command as the NextZXOS file handler for .ss3 .ss4 files, first add it to the <kbd>c:/dot</kbd> directory and then add the following line to your <kbd>c:/nextzxos/browser.cfg</kbd> file:

    SS3,SS4,SSX<.ssx |

The viewer can also be invoked from the command line with <kbd>.ssx example.ss4</kbd>. Note that quote marks around the file name are not accepted. To use the viewer in a program use <kbd>.$</kbd>, as follows:</p>

      10 a$="example.ss4"
      20 .$ ssx a$
      
<h2>Screen type detection</h2>

Files with names ending with a "4" will be treated as MODE 4 (and those with a "3" as MODE 3). Alternatively you can pass "3" or "4" as a parameter after the filename (separated by a single space).

      10 a$="logo 4"
      20 .$ ssx a$

The ".ssx" file standard uses the total file size to determine the type, so files of exactly 24,580 bytes (24K of bitmap data + 4 palette bytes) will be treated as MODE 3. For anything else we'll just assume MODE 4.

💡 SAM's MODE 3 screens are 512x192 so I'm displaying them with Layer 2's 640x256 overscan mode, so it will require Next core 3.0.6 or later.

Files may optionally have a +3DOS header (if so it will be ignored).

<h2>Colours and line interrupt palette changes</h2>

<a href="https://www.worldofsam.org/products/screen-modes">SAM screens</a> are saved with palette information (usually 40 bytes) and optionally a series of palette changes to be made at certain screen lines. And hopefully a $FF byte as an end-marker. As mentioned, some files will truncate that down to just 4 bytes for MODE 3, and 16 bytes for MODE 4&mdash;and usually no end-marker.

For our purposes, the palette entries are mapped across to Next's 9-bit RGB colour space using a lookup table. (I'm grateful to Simon N Goodwin for helping me to interpret the "half bright" bit correctly). The line palette changes are implemented as Copper instructions. (This is not really necessary on Next and in a future version we could remap the bitmap data line-by-line to use all 128 SAM colours with no need for dynamic changes). FLASHing colours are not supported.

The BORDER colour (Next ULA palette index 16) is set to match the first colour-index of the image.

On SAM, the palette changes can extend into the BORDER but in this case the border area <em>will not</em> be affected by the Copper palette changes. (The Copper's attention is directed to Layer 2, and the border is part of Layer 0). MODE 3 screens, however, use only 512x192 of the full 640x256 so the remaining space around the edges does give a pseudo-border that *is* subject to palette changes.

<img src="https://robertmorrison.me/spectrum/next/samscreen/enceladus.jpeg" width="320" height="479" alt="Enceladus disk magazine advert shown on a flatscreen VGA monitor being driven by a ZX Spectrum Next. In front of the computer is a colour table print-out and a scientific calculator.">

<h2>BASIC demo</h2>

Here's a BASIC program that demonstrates the viewer by loading random MODE 4 screens from ZXArt.ee:

<img src="https://robertmorrison.me/public/files/tap.png">
<a href="bas/zxart.tap">zxart.tap</a> | <a href="bas/zxart.bas">zxart.bas</a> | <a href="bas/zxart.txt">zxart.txt</a>

    #program zxart.bas
    #autostart
      10 REM fetch/display images from ZXArt
      20 h$="tnfs.robertmorrison.me"
      30 u$="/api/zxart/sam4/random.php"
      40 f$= "/tmp/zxart.ss4"
      50 CLS : PRINT "loading..."
      60 .http -h h$ -u u$ -f f$
      70 .$ ssx f$
      80 PAUSE 0: GO TO %50
    9090 SAVE "zxart.bas" LINE %10
