/*
 * gs.h
 *
 * Copyright (C) 2004 Jesper Svennevid, Daniel Collin
 *
 * Licensed under the AFL v2.0. See the file LICENSE included with this
 * distribution for licensing terms.
 *
 */

#ifndef _PDK_DMAREGS_H_
#define _PDK_DMAREGS_H_

#ifdef __cplusplus
extern "C" {
#endif

#define D0_CHCR  ((volatile u32 *)(0x10008000))
#define D0_MADR  ((volatile u32 *)(0x10008010))
#define D0_QWC   ((volatile u32 *)(0x10008020))
#define D0_TADR  ((volatile u32 *)(0x10008030))
#define D0_ASR0  ((volatile u32 *)(0x10008040))
#define D0_ASR1  ((volatile u32 *)(0x10008050))

#define D1_CHCR  ((volatile u32 *)(0x10009000))
#define D1_MADR  ((volatile u32 *)(0x10009010))
#define D1_QWC   ((volatile u32 *)(0x10009020))
#define D1_TADR  ((volatile u32 *)(0x10009030))
#define D1_ASR0  ((volatile u32 *)(0x10009040))
#define D1_ASR1  ((volatile u32 *)(0x10009050))

#define D2_CHCR  ((volatile u32 *)(0x1000a000))
#define D2_MADR  ((volatile u32 *)(0x1000a010))
#define D2_QWC   ((volatile u32 *)(0x1000a020))
#define D2_TADR  ((volatile u32 *)(0x1000a030))
#define D2_ASR0  ((volatile u32 *)(0x1000a040))
#define D2_ASR1  ((volatile u32 *)(0x1000a050))

#define D3_CHCR  ((volatile u32 *)(0x1000b000))
#define D3_MADR  ((volatile u32 *)(0x1000b010))
#define D3_QWC   ((volatile u32 *)(0x1000b020))

#define D4_CHCR  ((volatile u32 *)(0x1000b400))
#define D4_MADR  ((volatile u32 *)(0x1000b410))
#define D4_QWC   ((volatile u32 *)(0x1000b420))
#define D4_TADR  ((volatile u32 *)(0x1000b430))

#define D5_CHCR  ((volatile u32 *)(0x1000c000))
#define D5_MADR  ((volatile u32 *)(0x1000c010))
#define D5_QWC   ((volatile u32 *)(0x1000c020))

#define D6_CHCR  ((volatile u32 *)(0x1000c400))
#define D6_MADR  ((volatile u32 *)(0x1000c410))
#define D6_QWC   ((volatile u32 *)(0x1000c420))
#define D6_TADR  ((volatile u32 *)(0x1000c430))

#define D7_CHCR  ((volatile u32 *)(0x1000c800))
#define D7_MADR  ((volatile u32 *)(0x1000c810))
#define D7_QWC   ((volatile u32 *)(0x1000c820))

#define D8_CHCR  ((volatile u32 *)(0x1000d000))
#define D8_MADR  ((volatile u32 *)(0x1000d010))
#define D8_QWC   ((volatile u32 *)(0x1000d020))
#define D8_SADR  ((volatile u32 *)(0x1000d080))

#define D9_CHCR  ((volatile u32 *)(0x1000d400))
#define D9_MADR  ((volatile u32 *)(0x1000d410))
#define D9_QWC   ((volatile u32 *)(0x1000d420))
#define D9_TADR  ((volatile u32 *)(0x1000d430))
#define D9_SADR  ((volatile u32 *)(0x1000d480))

#define D_STAT   ((volatile u32 *)(0x1000e010))
#define D_CTRL   ((volatile u32 *)(0x1000e000))

#ifdef __cplusplus
}
#endif

#endif // _PDK_DMAREGS_H_


