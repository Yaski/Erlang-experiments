using UnityEngine;
using System.Collections;
using System;

public class DataConverter : MonoBehaviour {

	public static float ReadFloat(byte[] bytes, int pos)
	{
		var buffer = new byte[]{bytes [pos + 3], bytes [pos + 2], bytes [pos + 1], bytes [pos] };

		return BitConverter.ToSingle(buffer, 0);
	}

	public static void WriteFloat(byte[] bytes, int pos, float x)
	{
		var bs = BitConverter.GetBytes(x);
		
		bytes[pos] = bs[3];
		bytes[pos + 1] = bs[2];
		bytes[pos + 2] = bs[1];
		bytes[pos + 3] = bs[0];
	}

}
