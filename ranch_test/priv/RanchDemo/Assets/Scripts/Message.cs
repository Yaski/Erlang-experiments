using UnityEngine;
using System.Collections;
using System.IO;
using System;

public class Message {
	public ushort length { get; set; }
	public byte[] content { get; set; }

	public Message(byte[] data) {
		content = data;
	}

	public static Message ReadFromStream(BinaryReader reader) {
		ushort len;
		byte[] len_buf;
		byte[] buffer;
		
		len_buf = reader.ReadBytes(2);
		if (BitConverter.IsLittleEndian) {
			ReverseBuff2(len_buf);
		}
		len = BitConverter.ToUInt16(len_buf, 0);
		
		buffer = reader.ReadBytes(len);
		
		return new Message(buffer);
	}
	
	public void WriteToStream(BinaryWriter writer) {
		length = (ushort) content.Length;
		byte[] len_bytes = BitConverter.GetBytes(length);
		
		if (BitConverter.IsLittleEndian) {
			ReverseBuff2(len_bytes);
		}
		writer.Write(len_bytes);
		
		writer.Write(content);
	}

	private static void ReverseBuff2(byte[] buf)
	{
		byte tmp = buf[0];
		buf[0] = buf[1];
		buf[1] = tmp;
	}

}
