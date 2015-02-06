#include <jni.h>
#include <sys/time.h>
#include <time.h>
#include <android/log.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <assert.h>
#include <string.h>
#include <pthread.h>

// for native audio
#include <SLES/OpenSLES.h>
#include <SLES/OpenSLES_Android.h>

//////////////////////////////////////////////77
#include "engine/engine.h"
#include "fluxa/Graph.h"

#include "audio.h"

// audio nonsense
// engine interfaces
static SLObjectItf engineObject = NULL;
static SLEngineItf engineEngine;

// output mix interfaces
static SLObjectItf outputMixObject = NULL;

// buffer queue player interfaces
static SLObjectItf bqPlayerObject = NULL;
static SLPlayItf bqPlayerPlay;
static SLAndroidSimpleBufferQueueItf bqPlayerBufferQueue;
static SLEffectSendItf bqPlayerEffectSend;
static SLMuteSoloItf bqPlayerMuteSolo;
static SLVolumeItf bqPlayerVolume;

// recorder interfaces
static SLObjectItf recorderObject = NULL;
static SLRecordItf recorderRecord;
static SLAndroidSimpleBufferQueueItf recorderBufferQueue;

#define RECORDER_FRAMES (160)
static short recorderBufferA[RECORDER_FRAMES];
static short recorderBufferB[RECORDER_FRAMES];
static short processBuffer[RECORDER_FRAMES];
static unsigned recorderSize = 0;
static int current_buffer=-1;

unsigned int noise_size=1000;
unsigned short *noise;
unsigned int size;


Sample *out_left;
Sample *out_right;
Graph *graph;

void audio_init()
{
    WaveTable::WriteWaves();

    unsigned int s=RECORDER_FRAMES;
    out_left=new Sample(s);
    out_right=new Sample(s);
    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "inner init"); 
    size=s;
    graph=engine::get()->get_audio_graph();
    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "graph is %d", (int)graph); 
}

void audio_process(short *data)
{
    out_left->Zero();
    out_right->Zero();

    graph->Process(size/2,*out_left,*out_right);

    unsigned int pos=0;
    for (unsigned int i=0; i<size; i+=2)
    {
        data[i]=(short)((*out_left)[pos++]*3276);
    }   
    pos=0;
    for (unsigned int i=1; i<size; i+=2)
    {
        data[i]=(short)((*out_right)[pos++]*3276);
    }   
}

// this callback handler is called every time a buffer finishes recording
void bqRecorderCallback(SLAndroidSimpleBufferQueueItf bq, void *context)
{
    SLresult result; 
    current_buffer++;
    if (current_buffer>1) current_buffer=0;
    short *next_buffer=recorderBufferA;
    if (current_buffer==1) next_buffer=recorderBufferB;

    result = (*recorderBufferQueue)->Enqueue(recorderBufferQueue, next_buffer,
                                             RECORDER_FRAMES * sizeof(short));
}


void bqPlayerCallback(SLAndroidSimpleBufferQueueItf bq, void *context) 
{ 
    short *this_buffer=recorderBufferA;
    if (current_buffer==0) this_buffer=recorderBufferB;    
    memcpy(processBuffer,this_buffer,RECORDER_FRAMES * sizeof(short));
    audio_process(processBuffer);

    SLresult result; // enqueue another buffer 
    result = (*bqPlayerBufferQueue)->Enqueue(bqPlayerBufferQueue, processBuffer, RECORDER_FRAMES*sizeof(short));
} 

pthread_t audioThread;
void create_player();

void *AudioInOutThread( void *ptr ) 
{ 
    SLresult result; 
    create_player(); 

    result = (*bqPlayerBufferQueue)->Enqueue(bqPlayerBufferQueue, recorderBufferA, RECORDER_FRAMES*sizeof(short));
    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "enqueue saw play %d", result); 

// set the player's state to playing 
    result = (*bqPlayerPlay)->SetPlayState(bqPlayerPlay, SL_PLAYSTATE_PLAYING); 
    assert(SL_RESULT_SUCCESS == result); 

    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "started playing %d", result); 

} 

jboolean setup_recorder()
{
    SLresult result;

    // configure audio source
    SLDataLocator_IODevice loc_dev = {SL_DATALOCATOR_IODEVICE, SL_IODEVICE_AUDIOINPUT,
            SL_DEFAULTDEVICEID_AUDIOINPUT, NULL};
    SLDataSource audioSrc = {&loc_dev, NULL};

    // configure audio sink
    SLDataLocator_AndroidSimpleBufferQueue loc_bq = {SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE, 2};
    SLDataFormat_PCM format_pcm = {SL_DATAFORMAT_PCM, 1, SL_SAMPLINGRATE_16,
        SL_PCMSAMPLEFORMAT_FIXED_16, SL_PCMSAMPLEFORMAT_FIXED_16,
        SL_SPEAKER_FRONT_CENTER, SL_BYTEORDER_LITTLEENDIAN};
    SLDataSink audioSnk = {&loc_bq, &format_pcm};

    // create audio recorder
    // (requires the RECORD_AUDIO permission)
    const SLInterfaceID id[1] = {SL_IID_ANDROIDSIMPLEBUFFERQUEUE};
    const SLboolean req[1] = {SL_BOOLEAN_TRUE};
    result = (*engineEngine)->CreateAudioRecorder(engineEngine, &recorderObject, &audioSrc,
            &audioSnk, 1, id, req);
    if (SL_RESULT_SUCCESS != result) {
        return JNI_FALSE;
    }

    // realize the audio recorder
    result = (*recorderObject)->Realize(recorderObject, SL_BOOLEAN_FALSE);
    if (SL_RESULT_SUCCESS != result) {
        return JNI_FALSE;
    }

    // get the record interface
    result = (*recorderObject)->GetInterface(recorderObject, SL_IID_RECORD, &recorderRecord);
    assert(SL_RESULT_SUCCESS == result);

    // get the buffer queue interface
    result = (*recorderObject)->GetInterface(recorderObject, SL_IID_ANDROIDSIMPLEBUFFERQUEUE,
            &recorderBufferQueue);
    assert(SL_RESULT_SUCCESS == result);

    // register callback on the buffer queue
    result = (*recorderBufferQueue)->RegisterCallback(recorderBufferQueue, bqRecorderCallback,
            NULL);
    assert(SL_RESULT_SUCCESS == result);

    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "recording set up %d", result); 


    return JNI_TRUE;
}



// create buffer queue audio player
void create_player()
{
    SLresult result;

    // configure audio source
    SLDataLocator_AndroidSimpleBufferQueue loc_bufq = {SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE, 2};
    SLDataFormat_PCM format_pcm = {SL_DATAFORMAT_PCM, 1, SL_SAMPLINGRATE_16,
        SL_PCMSAMPLEFORMAT_FIXED_16, SL_PCMSAMPLEFORMAT_FIXED_16,
        SL_SPEAKER_FRONT_CENTER, SL_BYTEORDER_LITTLEENDIAN};
    SLDataSource audioSrc = {&loc_bufq, &format_pcm};

    // configure audio sink
    SLDataLocator_OutputMix loc_outmix = {SL_DATALOCATOR_OUTPUTMIX, outputMixObject};
    SLDataSink audioSnk = {&loc_outmix, NULL};

    // create audio player
    const SLInterfaceID ids[3] = {SL_IID_BUFFERQUEUE, SL_IID_EFFECTSEND,
            /*SL_IID_MUTESOLO,*/ SL_IID_VOLUME};
    const SLboolean req[3] = {SL_BOOLEAN_TRUE, SL_BOOLEAN_TRUE,
            /*SL_BOOLEAN_TRUE,*/ SL_BOOLEAN_TRUE};
    result = (*engineEngine)->CreateAudioPlayer(engineEngine, &bqPlayerObject, &audioSrc, &audioSnk,
            3, ids, req);
    assert(SL_RESULT_SUCCESS == result);

    // realize the player
    result = (*bqPlayerObject)->Realize(bqPlayerObject, SL_BOOLEAN_FALSE);
    assert(SL_RESULT_SUCCESS == result);

    // get the play interface
    result = (*bqPlayerObject)->GetInterface(bqPlayerObject, SL_IID_PLAY, &bqPlayerPlay);
    assert(SL_RESULT_SUCCESS == result);

    // get the buffer queue interface
    result = (*bqPlayerObject)->GetInterface(bqPlayerObject, SL_IID_BUFFERQUEUE,
            &bqPlayerBufferQueue);
    assert(SL_RESULT_SUCCESS == result);

    // register callback on the buffer queue
    result = (*bqPlayerBufferQueue)->RegisterCallback(bqPlayerBufferQueue, bqPlayerCallback, NULL);
    assert(SL_RESULT_SUCCESS == result);

    // get the effect send interface
    result = (*bqPlayerObject)->GetInterface(bqPlayerObject, SL_IID_EFFECTSEND,
            &bqPlayerEffectSend);
    assert(SL_RESULT_SUCCESS == result);

#if 0   // mute/solo is not supported for sources that are known to be mono, as this is
    // get the mute/solo interface
    result = (*bqPlayerObject)->GetInterface(bqPlayerObject, SL_IID_MUTESOLO, &bqPlayerMuteSolo);
    assert(SL_RESULT_SUCCESS == result);
#endif

    // get the volume interface
    result = (*bqPlayerObject)->GetInterface(bqPlayerObject, SL_IID_VOLUME, &bqPlayerVolume);
    assert(SL_RESULT_SUCCESS == result);

    // set the player's state to playing
    result = (*bqPlayerPlay)->SetPlayState(bqPlayerPlay, SL_PLAYSTATE_PLAYING);
    assert(SL_RESULT_SUCCESS == result);

    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "player setup %d", result); 


}

void create_audio_engine()
{
    SLresult result;
    
// create engine 
    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "initEngine");  
    SLEngineOption EngineOption[] = {(SLuint32) SL_ENGINEOPTION_THREADSAFE, (SLuint32) SL_BOOLEAN_TRUE}; 
    result = slCreateEngine( &engineObject, 1, EngineOption, 0, NULL, NULL); 
    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "create engine result %d", result); 
    assert(SL_RESULT_SUCCESS == result); 

// realize the engine 
    result = (*engineObject)->Realize(engineObject, SL_BOOLEAN_FALSE); 
    assert(SL_RESULT_SUCCESS == result); 

// get the engine interface, which is needed in order to create other objects 
    result = (*engineObject)->GetInterface(engineObject, SL_IID_ENGINE, &engineEngine); 
    assert(SL_RESULT_SUCCESS == result); 
    
    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "get engine result %d", result); 

    // create output mix, with environmental reverb specified as a non-required interface
    const SLInterfaceID ids[1] = {SL_IID_ENVIRONMENTALREVERB};
    const SLboolean req[1] = {SL_BOOLEAN_FALSE};
    result = (*engineEngine)->CreateOutputMix(engineEngine, &outputMixObject, 1, ids, req);
    assert(SL_RESULT_SUCCESS == result);

    // realize the output mix
    result = (*outputMixObject)->Realize(outputMixObject, SL_BOOLEAN_FALSE);
    assert(SL_RESULT_SUCCESS == result);

    result = pthread_create( &audioThread, NULL, AudioInOutThread, NULL); 

    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "created audio playback thread %d", result); 

    setup_recorder();

    // the buffer is not valid for playback yet
    recorderSize = 0;

    // enqueue an empty buffer to be filled by the recorder
    // (for streaming recording, we would enqueue at least 2 empty buffers to start things off)
    result = (*recorderBufferQueue)->Enqueue(recorderBufferQueue, recorderBufferA,
                                             RECORDER_FRAMES * sizeof(short));

    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "enqueue record A %d", result); 

    // start recording
    result = (*recorderRecord)->SetRecordState(recorderRecord, SL_RECORDSTATE_RECORDING);
    assert(SL_RESULT_SUCCESS == result);

    __android_log_print(ANDROID_LOG_INFO, "NativeAudio", "started recording %d", result); 
}
