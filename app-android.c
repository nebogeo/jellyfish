#include <jni.h>
#include <sys/time.h>
#include <time.h>
#include <android/log.h>
#include <stdint.h>
#include <stdio.h>
#include "scheme/scheme.h"

static int  sWindowWidth  = 320;
static int  sWindowHeight = 480;

void Java_foam_starwisp_Scheme_nativeInit(JNIEnv* env)
{
    appInit();
}

/* Call to initialize the graphics state */
void Java_foam_starwisp_Scheme_nativeInitGL( JNIEnv*  env )
{
    importGLInit();
    initGL();
}

void Java_foam_starwisp_Scheme_nativeResize( JNIEnv*  env, jobject  thiz, jint w, jint h )
{
    sWindowWidth  = w;
    sWindowHeight = h;
    __android_log_print(ANDROID_LOG_INFO, "SanAngeles", "resize w=%d h=%d", w, h);
}


void Java_foam_starwisp_Scheme_nativeDone(JNIEnv* env)
{
    appDeinit();
    importGLDeinit();
}

void Java_foam_starwisp_Scheme_nativeRender( JNIEnv*  env )
{
    appRender(sWindowWidth, sWindowHeight);
}


jstring Java_foam_starwisp_Scheme_nativeEval(JNIEnv* env, jobject thiz, jstring code)
{
   const char *native_code = (*env)->GetStringUTFChars(env, code, 0);
   appEval(native_code);
   (*env)->ReleaseStringUTFChars(env, code, native_code);
   if (starwisp_data!=NULL) {
       jstring ret = (*env)->NewStringUTF(env,starwisp_data);
       free(starwisp_data);
       starwisp_data=NULL;
       return ret;
   }
   return (*env)->NewStringUTF(env,"");
}

void Java_foam_starwisp_Scheme_nativeLoadTexture(JNIEnv* env, jobject thiz, jstring texname, jbyteArray arr, jint w, jint h)
{
    char *data = (char *) (*env)->GetByteArrayElements(env,arr,NULL);
    int len = (*env)->GetArrayLength(env, arr);
    const char *filename = (*env)->GetStringUTFChars(env, texname, 0);

    __android_log_print(ANDROID_LOG_INFO, "starwisp", "loading texture");


    int id=appLoadTexture(filename,w,h,data);

    __android_log_print(ANDROID_LOG_INFO, "starwisp", "loaded texture");

    (*env)->ReleaseStringUTFChars(env, texname, filename);
    (*env)->ReleaseByteArrayElements(env,arr,data,JNI_ABORT);
}

// create the engine and output mix objects
void Java_foam_starwisp_Scheme_createEngine(JNIEnv* env, jclass clazz)
{
    audio_init();
    create_audio_engine();
}
