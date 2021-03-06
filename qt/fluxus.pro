######################################################################
# Automatically generated by qmake (2.01a) Sun Jul 5 17:49:45 2015
######################################################################

TEMPLATE = app
TARGET =
DEPENDPATH += . 2
INCLUDEPATH += . 2

QT += core gui opengl

# Input
HEADERS += brick_widget.h \
           brick_list_widget.h \
           brick_atom_widget.h \
           brick_atom_slider_widget.h \
           canvas_widget.h \
           sexpr.h \
           drag_widget.h \
           GLGraphicsScene.h \
           MainWindow.h \


SOURCES += ../src/graphics.cpp\
		   ../src/audio.cpp\
		   ../src/interpreter.cpp\
	 	   ../src/core/fixed.cpp\
           ../src/core/list.cpp\
           ../src/core/idmap.cpp\
           ../src/scheme/scheme.cpp\
		   ../src/scheme/interface.cpp\
           ../src/core/geometry.cpp\
           ../src/core/noise.cpp\
           ../src/core/pixels.cpp\
           ../src/core/osc.cpp\
	       ../src/core/stream.cpp\
           ../src/fluxa/sample.cpp\
           ../src/fluxa/allocator.cpp\
           ../src/fluxa/graph.cpp\
           ../src/fluxa/graph_node.cpp\
           ../src/fluxa/modules.cpp\
           ../src/fluxa/module_nodes.cpp\
           ../src/fluxa/event.cpp\
           ../src/fluxa/event_queue.cpp\
           ../src/fluxa/time.cpp\
		   ../src/fluxa/ring_buffer.cpp\
		   ../src/fluxa/command_ring_buffer.cpp\
		   ../src/fluxa/OSC_server.cpp \
		   ../src/audio/portaudio_client.cpp \
           ../src/engine/obj_reader.cpp\
           ../src/engine/engine.cpp\
           ../src/engine/primitive.cpp\
           ../src/engine/text_primitive.cpp\
           ../src/engine/scenegraph.cpp\
           ../src/engine/scenenode.cpp\
           ../src/engine/texture.cpp\
           # ../src/engine/shader.cpp\
           ../src/engine/nomadic.cpp\
           ../src/engine/jellyfish_primitive.cpp\
           ../src/engine/jellyfish.cpp\
	  	   ../src/linux/glut_graphics.cpp\
           #../src/rpi/graphics.cpp\
	       #../src/rpi/input.cpp\
           brick_widget.cpp \
           brick_list_widget.cpp \
           brick_atom_widget.cpp \
           brick_atom_slider_widget.cpp \
           canvas_widget.cpp \
           drag_widget.cpp \
           sexpr.cpp \
           GLGraphicsScene.cpp \
           main.cpp \
           MainWindow.cpp \


# jellyfish stuff
DEFINES += QT_BUILD
INCLUDEPATH += ../src
LIBS += -lsndfile -lportaudio -ljpeg -llo -ldl -lpthread -lpng -lGL -lm
# linux only
#LIBS +=  -lglut
QMAKE_CXXFLAGS += -Wfatal-errors -Wno-write-strings -Wno-unused

#rpi stuff
#DEFINES += FLX_RPI
#INCLUDEPATH += /opt/vc/include/
#INCLUDEPATH += /opt/vc/include/interface/vcos/pthreads/
#INCLUDEPATH += /opt/vc/include/interface/vmcs_host/linux/
#LIBS += -L/opt/vc/lib -lEGL -lGLESv1_CM -lbcm_host


# assets
DEFINES += ASSETS_PATH=\\\"../assets/\\\"
RESOURCES     = application.qrc
