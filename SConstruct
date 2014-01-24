target = 'jellyfish'
platform = ARGUMENTS.get('TARGET','LINUX')
env = Environment(CCFLAGS='-O3 -std=gnu++0x -ggdb -DUSE_MATH=1 -Wno-write-strings')
source = ['main.cpp',
          'core/fixed.cpp',
          'core/list.cpp',
          'core/idmap.cpp',
          'scheme/scheme.cpp',
          'core/geometry.cpp',
          'core/noise.cpp',
          'fluxa/Sample.cpp',
          'fluxa/Allocator.cpp',
          'fluxa/Graph.cpp',
          'fluxa/GraphNode.cpp',
          'fluxa/Modules.cpp',
          'fluxa/ModuleNodes.cpp',
          'engine/obj_reader.cpp',
          'engine/engine.cpp',
          'engine/primitive.cpp',
          'engine/text_primitive.cpp',
          'engine/scenegraph.cpp',
          'engine/scenenode.cpp',
          'engine/texture.cpp',
          'engine/nomadic.cpp',
          'engine/jellyfish_primitive.cpp',
          'engine/jellyfish.cpp'
       ]

if platform=='LINUX':
   env.Append(LIBS = ['glut', 'GL', 'png', 'pthread', 'dl', 'lo'])
   env.Append(CCFLAGS=' -fpermissive -DFLX_LINUX')
   env.Append(CPPPATH = '.')
   source.append(['core/db.cpp',
                  'core/db_container.cpp',
                  'sqlite/sqlite3.c',])

if platform=='RPI':
   # raspberry pi
   env.Append(LIBS = ['GLESv1_CM', 'EGL', 'bcm_host', 'X11', 'png', 'lo'])
   env.Append(CCFLAGS=' -DFLX_RPI -fpermissive')
   env.Append(CPPPATH = '/opt/vc/include/interface/vcos/pthreads/:/opt/vc/include/interface/vmcs_host/linux:/opt/vc/include/:.')
   env.Append(LIBPATH = '/opt/vc/lib')

env.Program(target=target, source=source)
