// Copyright (C) 2008 Dave Griffiths
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

#include "obj_reader.h"

#include <cstdlib>
#include <cstdio>
#include <algorithm>
#include <cstring>

#include "assert.h"


using namespace std;

obj_reader::obj_reader()
{
}

obj_reader::~obj_reader()
{
	// clear some mem
	m_Position.clear();
	m_Texture.clear();
	m_Normal.clear();
	m_Faces.clear();
}

void obj_reader::FormatRead(const string &filename)
{
	FILE *file = fopen(filename.c_str(),"r");
	if (file==NULL)
	{
		printf("Cannot open .obj file %s",filename.c_str());
	}

	fseek(file,0,SEEK_END);
	m_DataSize = ftell(file);
	rewind(file);

	m_Data = new char[m_DataSize+1];
	if (m_DataSize!=fread(m_Data,1,m_DataSize,file))
	{
		printf("Error reading .obj file %s",filename.c_str());
		fclose(file);
	}
	fclose(file);
	m_Data[m_DataSize]='\0';

    RawRead(m_Data);

	// now get rid of the text
	delete[] m_Data;

}

void obj_reader::RawRead(char *data)
{
    m_Data=data;
    m_DataSize=strlen(m_Data);

	m_UnifiedIndices = true;
	ReadOBJ(m_Position, m_Texture, m_Normal, m_Faces);

	// skip processing if all the indices are the same per vertex
	if (m_UnifiedIndices)
	{
		m_Indices.clear();
		for (vector<Face>::const_iterator fi=m_Faces.begin();
				fi!=m_Faces.end(); ++fi)
		{
			for (vector<Indices>::const_iterator ii=fi->Index.begin();
					ii!=fi->Index.end(); ++ii)
			{
				m_Indices.push_back(ii->Position);
			}
		}
	}
	else
	{
		// shuffle stuff around so we only have one set of indices
		vector<Indices> unique=RemoveDuplicateIndices();
		ReorderData(unique);
		UnifyIndices(unique);
	}
}

unsigned int obj_reader::TokeniseLine(unsigned int pos, vector<string> &output)
{
	char c=m_Data[pos];
	vector<string> temp;
	temp.push_back("");
	while(c!='\n' && pos<m_DataSize)
	{
		if (c==' ' && *temp.rbegin()!="") temp.push_back("");
		else temp.rbegin()->push_back(c);
		c=m_Data[++pos];
	}

	// get rid of whitespace
	output.clear();
	for(vector<string>::iterator i=temp.begin(); i!=temp.end(); ++i)
	{
		if (*i!="")	output.push_back(*i);
	}

	return pos+1;
}

void obj_reader::TokeniseIndices(const string &str, vector<string> &output)
{
	unsigned int pos=0;
	output.clear();
	output.push_back("");
	while(pos<str.size())
	{
		char c=str[pos++];
		if (c==' ' || c=='/') output.push_back("");
		else output.rbegin()->push_back(c);
	}
}

void obj_reader::ReadOBJ(std::vector<vec3> &positions,
		std::vector<vec3> &textures,
		std::vector<vec3> &normals,
		std::vector<Face> &faces)
{
	unsigned int pos=0;
	positions.clear();
	textures.clear();
	normals.clear();
	faces.clear();

	while (pos<m_DataSize)
	{
		vector<string> tokens;
		pos = TokeniseLine(pos, tokens);
		if (tokens.empty())
			continue;

		if ((tokens[0] == "v") && (tokens.size() == 4))
		{
			positions.push_back(vec3(atof(tokens[1].c_str()),
									 atof(tokens[2].c_str()),
									 atof(tokens[3].c_str())));
		}
		else if (tokens[0] == "vt")
		{
			if (tokens.size() == 4)
			{
				textures.push_back(vec3(atof(tokens[1].c_str()),
										 atof(tokens[2].c_str()),
										 atof(tokens[3].c_str())));
			}
			else if (tokens.size() == 3)
			{
				textures.push_back(vec3(atof(tokens[1].c_str()),
										 atof(tokens[2].c_str()),
										 0));
			}
		}
		else if ((tokens[0] == "vn") && (tokens.size() == 4))
		{
			normals.push_back(vec3(atof(tokens[1].c_str()),
									 atof(tokens[2].c_str()),
									 atof(tokens[3].c_str())));
		}
		else if (tokens[0] == "f")
		{
			Face f;
			for(unsigned int i=1; i<tokens.size(); i++)
			{
				vector<string> itokens;
				TokeniseIndices(tokens[i],itokens);
				if (itokens.size()==3)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					if (itokens[1]!="") ind.Texture=(unsigned int)atof(itokens[1].c_str())-1;
					if (itokens[2]!="") ind.Normal=(unsigned int)atof(itokens[2].c_str())-1;
					f.Index.push_back(ind);

					if ((ind.Position != ind.Texture) ||
						(ind.Position != ind.Normal) ||
						(ind.Texture != ind.Normal))
					{
						m_UnifiedIndices = false;
					}
				}
				else if (itokens.size()==2)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					if (itokens[1]!="") ind.Texture=(unsigned int)atof(itokens[1].c_str())-1;
					f.Index.push_back(ind);

					if (ind.Position != ind.Texture)
					{
						m_UnifiedIndices = false;
					}
				}
				else if (itokens.size()==1)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					f.Index.push_back(ind);
				}
				else
				{
					printf("Wrong number of indices in .obj file (%d)",tokens.size());
				}
			}

			// subdivide polygons to triangles
			if (f.Index.size() > 3)
			{
				Face tri;
				for (unsigned i = 0; i < 3; i++)
				{
					tri.Index.push_back(f.Index[i]);
				}
				faces.push_back(tri);

				for (unsigned i = 3; i < f.Index.size(); i++)
				{
					tri.Index.erase(tri.Index.begin() + 1);
					tri.Index.push_back(f.Index[i]);
					faces.push_back(tri);
				}
			}
			else
			{
				faces.push_back(f);
			}
		}
	}
}

vector<obj_reader::Indices> obj_reader::RemoveDuplicateIndices()
{
	vector<Indices> ret;
	for (vector<Face>::iterator fi=m_Faces.begin();
		fi!=m_Faces.end(); ++fi)
	{
		for (vector<Indices>::iterator ii=fi->Index.begin();
			ii!=fi->Index.end(); ++ii)
		{
			vector<Indices>::iterator result = find(ret.begin(), ret.end(), *ii);
			if (result == ret.end())
			{
				ii->UnifiedIndex = ret.size();
				ret.push_back(*ii);
			}
			else
			{
				ii->UnifiedIndex = result - ret.begin();
			}
		}
	}
	return ret;
}

void obj_reader::ReorderData(const vector<obj_reader::Indices> &unique)
{
	vector<vec3> NewPosition;
	vector<vec3> NewTexture;
	vector<vec3> NewNormal;

	for (vector<Indices>::const_iterator i=unique.begin();
		i!=unique.end(); ++i)
	{
		if (!m_Position.empty()) NewPosition.push_back(m_Position[i->Position]);
		if (!m_Texture.empty()) NewTexture.push_back(m_Texture[i->Texture]);
		if (!m_Normal.empty()) NewNormal.push_back(m_Normal[i->Normal]);
	}

	m_Position=NewPosition;
	m_Texture=NewTexture;
	m_Normal=NewNormal;
}

void obj_reader::UnifyIndices(const vector<Indices> &unique)
{
	m_Indices.clear();
	for (vector<Face>::const_iterator fi=m_Faces.begin();
		fi!=m_Faces.end(); ++fi)
	{
		for (vector<Indices>::const_iterator ii=fi->Index.begin();
			ii!=fi->Index.end(); ++ii)
		{
			m_Indices.push_back(ii->UnifiedIndex);
		}
	}
}
