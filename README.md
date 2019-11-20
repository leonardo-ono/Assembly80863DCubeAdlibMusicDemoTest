# Assembly80863DCubeAdlibMusicDemoTest
First assembly 8086 vga graphics 3D cube + adlib opl2 music demo test for OLOS (https://github.com/leonardo-ono/Assembly8086OLOS). Of course, it also work on DOS :) Video: https://youtu.be/XC3eRKOJQ3U

3d cube (fixed-point math) + adlib opl2 music demo 
written by Leonardo Ono (ono.leo@gmail.com)
november 19, 2019

target OS: OLOS & DOS

target machine: at least 486 66 MHz (although the code is 16 bits)

executable extension: *.EXE

assembler: masm 6.00B
use: masm demo3d
     link demo3d;
     
note: for OLOS, it is necessary to convert demo3d.exe to demo3d.com
assembler: nasm
use: nasm exe2com.asm -o demo3d.com -f bin


resources:
midi "stage 1 - vampire killer" - https://www.khinsider.com/midi/nes/castlevania


special thanks to:
------------------
* kvee - https://www.youtube.com/watch?v=WerFTLpz3b4&t=80s
* Peter De Wachter - https://github.com/pdewacht/adlipt
* Maarten Janssen -https://github.com/DhrBaksteen/ArduinoOPL2
* Mark Pilsur

these guys kindly answered my email clarifying a lot of questions about opl2 :)
