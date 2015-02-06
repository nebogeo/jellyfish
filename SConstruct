target = 'jellyfish'
platform = ARGUMENTS.get('TARGET','LINUX')
env = Environment(CCFLAGS='-O3 -std=gnu++0x -ggdb -DUSE_MATH=1 -Wno-write-strings')
source = ['src/main.cpp',
          'src/audio/alsa.cpp',
          'src/core/fixed.cpp',
          'src/core/list.cpp',
          'src/core/idmap.cpp',
          'src/scheme/scheme.cpp',
          'src/core/geometry.cpp',
          'src/core/noise.cpp',
          'src/fluxa/Sample.cpp',
          'src/fluxa/Allocator.cpp',
          'src/fluxa/Graph.cpp',
          'src/fluxa/GraphNode.cpp',
          'src/fluxa/Modules.cpp',
          'src/fluxa/ModuleNodes.cpp',
          'src/engine/obj_reader.cpp',
          'src/engine/engine.cpp',
          'src/engine/primitive.cpp',
          'src/engine/text_primitive.cpp',
          'src/engine/scenegraph.cpp',
          'src/engine/scenenode.cpp',
          'src/engine/texture.cpp',
          'src/engine/nomadic.cpp',
          'src/engine/jellyfish_primitive.cpp',
          'src/engine/jellyfish.cpp'
       ]

if platform=='LINUX':
   env.Append(LIBS = ['glut', 'GL', 'png', 'pthread', 'dl', 'lo', 'jpeg'])
   env.Append(CCFLAGS=' -fpermissive -DFLX_LINUX')
   env.Append(CPPPATH = '.')
   source.append(['core/db.cpp',
                  'core/db_container.cpp',
                  'sqlite/sqlite3.c',])

if platform=='RPI':
   # raspberry pi
   env.Append(LIBS = ['GLESv1_CM', 'EGL', 'bcm_host', 'X11', 'png', 'lo', 'asound'])
   env.Append(CCFLAGS=' -DFLX_RPI -fpermissive')
   env.Append(CPPPATH = '/opt/vc/include/interface/vcos/pthreads/:/opt/vc/include/interface/vmcs_host/linux:/opt/vc/include/:.:src/')
   env.Append(LIBPATH = '/opt/vc/lib')
   source.append(['src/rpi/input.cpp'])


env.Program(target=target, source=source)
