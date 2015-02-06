#include "core/vec3.h"
#include <vector>
#include <string>

using namespace std;

class obj_reader
{
public:
    obj_reader();
    ~obj_reader();

	class Indices
	{
	public:
		Indices() : Position(0), Texture(0), Normal(0), UnifiedIndex(0) {}

		bool operator==(const Indices &other) const
		{
			return Position==other.Position &&
				   Texture==other.Texture &&
				   Normal==other.Normal;
		}

		unsigned int Position;
		unsigned int Texture;
		unsigned int Normal;

		unsigned int UnifiedIndex;
	};

	struct Face
	{
        vector<Indices> Index;
	};

    void FormatRead(const string &filename);
    void RawRead(char *data);
	unsigned int TokeniseLine(unsigned int pos, vector<std::string> &output);
	void TokeniseIndices(const string &str, std::vector<std::string> &output);
	void ReadOBJ(std::vector<vec3> &positions,
				std::vector<vec3> &textures,
				std::vector<vec3> &normals,
				std::vector<Face> &faces);
	vector<Indices> RemoveDuplicateIndices();
	void ReorderData(const std::vector<Indices> &unique);
	void UnifyIndices(const std::vector<Indices> &unique);

	unsigned int m_DataSize;
	char *m_Data;

	std::vector<Face> m_Faces;
	std::vector<vec3> m_Position;
	std::vector<vec3> m_Texture;
	std::vector<vec3> m_Normal;
	std::vector<unsigned int> m_Indices;

	bool m_UnifiedIndices;
};
