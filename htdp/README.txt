
To RUN: 
-----------------------------------------------------------------------------

 $ make 

generate the draft or release version of the HtDP/2e HTML and the Notes. 
The script will not generate correct cross references when run the first
time, so it runs them twice. 

 $ make clean 

deletes the generated files that can go. 

CODE
-----------------------------------------------------------------------------
xhtml					generate HtDP/2e, refer to Notes 
					-- runs xnotes's main for first run
xnotes					generate Notes, refer to HtDP/2e
x-info.rkt				common functions and constants 
x-info.dat				a data file needed to bootstrap

INPUT FILES & DIRECTORIES
-----------------------------------------------------------------------------
0prologue.scrbl				
note-on-mice-and-characters.scrbl
note-on-teaching-part-I.scrbl
notes.scrbl
HtDP2e.scrbl

Images					images needed to run (and more)
Shared					the shared scribble auxiliaries

Trash/					this is where the HTML files goe

GENERATED FILES & DIRECTORIES
-----------------------------------------------------------------------------
Draft.scrbl				for the draft version 

info-HtDP2e.rktl			data files for cross referencing
info-HtDP2eDraft.rktl			data files for cross referencing
info-notes.rktl				data files for cross referencing
info-notesDraft.rktl			data files for cross referencing
