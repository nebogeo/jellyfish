#include "importgl.h"
#include "../core/fixed.h"
#include <stdlib.h>
#include <stdio.h>

typedef struct
{
// Handle to a program object
    GLuint programObject;
} UserData;

///
// Create a shader object, load the shader source, and
// compile the shader.
//

GLuint load_shader(const char *shaderSrc, GLenum type)
{
    GLuint shader;
    GLint compiled;
// Create the shader object
    shader = glCreateShader(type);
    if(shader == 0)
        return 0;
// Load the shader source
    glShaderSource(shader, 1, &shaderSrc, NULL);
// Compile the shader
    glCompileShader(shader);
// Check the compile status
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
    if(!compiled)
    {
        GLint infoLen = 0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLen);
        if(infoLen > 1)
        {
            char* infoLog = (char*)malloc(sizeof(char) * infoLen);
            glGetShaderInfoLog(shader, infoLen, NULL, infoLog);
            printf("Error compiling shader:\n%s\n", infoLog);
            free(infoLog);
        }
        glDeleteShader(shader);
        return 0;
    }
    return shader;
}

