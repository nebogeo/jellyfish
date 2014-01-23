 		.syntax new
		.name vu1_unlit		
		.vu
		.init_vf_all
		.init_vi_all
        --enter
        --endenter
        ; load the matrix row by row
        lq		world_screen_row0, 0(vi00)
		lq		world_screen_row1, 1(vi00)
		lq		world_screen_row2, 2(vi00)
		lq		world_screen_row3, 3(vi00)

        ; load the params and set the addresses for
        ; the giftag and vertex data
        lq      params, 4(vi00)
        iaddiu  giftag_addr, vi00, 5
   		iaddiu	vertex_data, vi00, 6

        ; move the vertex count to an integer 
        ; register so we can loop over it
        mtir    vertex_index, params[x]
vertex_loop:
        ; load the colour (just increments vertex_data)
        lqi     colour, (vertex_data++)
        ; load vertex position
        lq      vertex, 0(vertex_data)

        ; apply the transformation
        mul     acc, world_screen_row0, vertex[x]
        madd    acc, world_screen_row1, vertex[y]
        madd    acc, world_screen_row2, vertex[z]
        madd    vertex, world_screen_row3, vertex[w]
        div     q, vf00[w], vertex[w]
        mul.xyz vertex, vertex, q

        ; convert to fixed point
        ftoi4   vertex, vertex

        ; overwrite the old vertex with the transformed one
        sqi     vertex, (vertex_data++)

        ; decrement and loop
        iaddi   vertex_index, vertex_index, -1
        ibne    vertex_index, vi00, vertex_loop
        
        ; send to gs
        xgkick  giftag_addr

        --exit
        --endexit