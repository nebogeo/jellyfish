#ifndef APP_H_INCLUDED
#define APP_H_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

#define WINDOW_DEFAULT_WIDTH    640
#define WINDOW_DEFAULT_HEIGHT   480
#define WINDOW_BPP              16

extern void appInit();
extern void initGL();
extern void appDeinit();
extern void appEval(char *code);
extern void appRender(int width, int height);
extern unsigned int appLoadTexture(const char *filename, int width, int height, char* data);

#ifdef __cplusplus
}
#endif


#endif // !APP_H_INCLUDED
