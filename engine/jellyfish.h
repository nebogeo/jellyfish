// Copyright (C) 2010 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// jellyfish, out of wack vector processor

#ifndef JELLYF_MACHINE
#define JELLYF_MACHINE

#include <stdio.h>
#include "../core/types.h"
#include "../core/vec3.h"
#include "../fluxa/Graph.h"

// instruction set
#define NOP 0
#define JMP 1
#define JMZ 2
#define JLT 3
#define JGT 4
#define LDL 5
#define LDA 6
#define LDI 7
#define STA 8
#define STI 9
#define ADD 10
#define SUB 11
#define MUL 12
#define DIV 13
#define ABS 14
#define SCS 15
#define ATN 16
#define DOT 17
#define CRS 18
#define SQR 19
#define LEN 20
#define DUP 21
#define DRP 22
#define CMP 23
#define SHF 24
#define BLD 25
#define RET 26
#define DBG 27
#define NRM 28
#define ADDX 29
#define ADDY 30
#define ADDZ 31
#define SWP 32
#define RND 33
#define MULL 34
#define JMR 35
#define LDLV 36
#define LENSQ 37
#define NOISE 38
#define LDS 39
#define STS 40
#define MULV 41
#define SYNTH_CRT 42
#define SYNTH_CON 43
#define SYNTH_PLY 44
#define FLR 45

// registers
#define REG_CONTROL 0   // pc, cycles, stack
#define REG_GRAPHICS 1  // size, [type, tex]
#define REG_TX_TRANSLATE 2
#define REG_TX_ROTATEA 3
#define REG_TX_ROTATEB 4
#define REG_TX_ROTATEC 5
#define REG_SENSOR_ADDR 6
#define CODE_START 7

#define REG_STK 511 // oops hardcoded :/

class jellyfish {

public:
	jellyfish(vec3 *heap_ptr, u32 heap_size);
	~jellyfish();

	// global
	vec3 peek(s32 addr) const;
    s32 peekix(s32 addr) const;
    s32 peekiy(s32 addr) const;
    s32 peekiz(s32 addr) const;
    void pokex(s32 addr, flx_real v);
    void pokey(s32 addr, flx_real v);
    void pokez(s32 addr, flx_real v);
	void poke(s32 addr, const vec3 &data);
	bool is_instr(s32 addr) const;
	void set_instr(s32 addr,bool s);
	void print_instr(s32 addr) const;

    void push(const vec3 &data);
    vec3 pop();

	void run();
	void simple_dump() const;
	void pretty_dump() const;
    void trash();

private:

	vec3 *m_heap;
    u32 m_heap_size;
	bool *m_instruction;
    Graph *m_audio_graph;
};

#endif
