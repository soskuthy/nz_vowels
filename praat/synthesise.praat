form Synthesise
	sentence filename_in
	sentence filename_out
    real f0_max
	real f0_min
endform

#Create Sound from formula: "sound_1", 1, 0.0, 0.1, 44100, "0"

# read table
Read Table from comma-separated file... 'filename_in$'
Rename... formants_table

table_nrow = Get number of rows
dur = Get value... 'table_nrow' locations

for t from 1 to table_nrow
	time[t] = Get value: t, "locations"
	f1[t] = Get value: t, "f1"
	f2[t] = Get value: t, "f2"
	f3[t] = Get value: t, "f3"
	f4[t] = Get value: t, "f4"
	f5[t] = Get value: t, "f5"
	f6[t] = Get value: t, "f6"
	endfor

# generate source
Create PitchTier: "f0", 0.00, dur
Add point: 0.00, f0_max
Add point: dur, f0_min

To Sound (phonation): 44100, 0.6, 0.05, 0.7, 0.03, 3.0, 4.0, 0

# generate formants
Create FormantGrid: "filter", 0.0, dur, 8, 500, 1000, 60, 50
Remove formant points between: 1, 0.0, dur
Remove formant points between: 2, 0.0, dur
Remove formant points between: 3, 0.0, dur
Remove formant points between: 4, 0.0, dur
Remove formant points between: 5, 0.0, dur
Remove formant points between: 6, 0.0, dur

# populate with values
for t from 1 to table_nrow
    Add formant point: 1, time[t], f1[t]
    Add formant point: 2, time[t], f2[t]
    Add formant point: 3, time[t], f3[t]
    Add formant point: 4, time[t], f4[t]
    Add formant point: 5, time[t], f4[t]
    Add formant point: 6, time[t], f4[t]
endfor

# filter
plusObject: "Sound f0"
Filter

trim_point = Get nearest zero crossing: 1, dur

Extract part... 0.0 'trim_point' rectangular 1.0 0

Rename... actual_sound

new_dur = Get total duration

Create IntensityTier... intensity 0 dur
t0 = 0
t1 = dur*0.02
t2 = dur*0.92
t3 = dur*0.99
t4 = dur

Add point... t0 70
Add point... t1 80
Add point... t2 79
Add point... t3 75
Add point... t4 60

plusObject: "Sound actual_sound"
Multiply... 1

Play

Save as WAV file... 'filename_out$'