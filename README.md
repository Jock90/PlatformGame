# PlatformGame
An old Amiga 1200 platform designer utility I created back in the day. The files include a small (c40 screen) demo game. The utility isn't perfect and does need more work to polish things. There is also a working screen designer that I haven't uploaded yet. Sprite alien patterns are constructed via by another utility, which needs some work, however, the patterns are visible in lines 6056 onwards and the feature decode table is at lines 348 onwards.
My aims are to use VS-Code and the excellent Amiga Assembly to continue playing with the code, once I work out how it works again, an enjoyable challenge :) 

There are some nice features defining the gfx blocks and the features of the sprite driver, so it would be good to tidy things up here and expand the features. 


The utility will run on real A1200 or inside WinUAE. There is a small demo game included.

I use the following WinUAE configuration:-

Create a basic A1200 inside WinUAE, and save configuration...

![image](https://user-images.githubusercontent.com/126944365/226379892-97f3486d-bd91-4606-8483-33b28c51a226.png)

![image](https://user-images.githubusercontent.com/126944365/226380049-8978c082-c22a-414f-9bac-920d808273db.png)

![image](https://user-images.githubusercontent.com/126944365/226380137-4beaaf6d-1a9c-4ac8-93df-21e76d7f249a.png)

![image](https://user-images.githubusercontent.com/126944365/226380223-3c3d4819-94ff-430e-bd3c-cf3846bdeafd.png)

![image](https://user-images.githubusercontent.com/126944365/226380295-923433d5-2fc3-4d61-b09d-04310a1a598f.png)

Make sure to map the joystick and fire button to suitable PC keys.

Add a shared folder for this configuration to a folder in your PC. Place all the files here.
There is only one asm file ("newplatdes orig.asm"). You will need to amend the incbin paths at lines 7095 - 7099 and 7246 - 7249 to point to the shared PC folder you created above.

Install Workbench and any other utilities you need. AsmOne 1.3 and Reqtools are included.

Load the configuration and run.

When the workbench loads, start AsmOne, then...
  Allocate 500KB of Fast
  Press 'R' and navigate to the "newplatdes orig.asm" source file.
  Press RMB on your mouse to show AsmOne menu, highlight "Assembler" then "Assemble.." and make sure 'UCase=LCase' is ticked.
  Press the 'ESC' key to enter the source editor and make sure the incbins paths (the two groups at the end of the program described above) are directed to your PC folder holding the data files.
  Press "a" to assemble. Any issues with the incbin paths will show up here, correct if needed.
  Press 'j' to run the demo game.
  
  Keys "_/-" and "=/+" increment and decrement the demo game screens, though the player may fall to death most times, especially if the player is near the top of the screen (the player default positions need to be updated when using this screen jump method (works ok if playing the game for real)...ToDo) 
  
  
  
