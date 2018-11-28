form Concatenate to length
	sentence dirname
	sentence path_out
	real edge_window 0.2
	real pause_dur 1.0
endform

Create Strings as file list: "list", dirname$ + "*.wav"
number_of_files = Get number of strings


# silence at start
Create Sound from formula: "initial_window", 1, 0.0, edge_window, 44100, "0"
to_select[1] = selected()

# import sound files one by one
for i from 1 to number_of_files
	select Strings list
	file$ = Get string... 'i'
	path$ = dirname$ + file$
	Read from file... 'path$'
	j = 2*i
	to_select[j] = selected()
	Create Sound from formula: "pause", 1, 0.0, pause_dur, 44100, "0"
	j = 2*i + 1
	to_select[j] = selected()
endfor

# silence at end
Create Sound from formula: "final_window", 1, 0.0, edge_window, 44100, "0"
to_select[j] = selected()

select to_select[1]

for i from 2 to j
	sel = to_select[i]
	plusObject: sel
endfor

Concatenate

Save as WAV file... 'path_out$'
