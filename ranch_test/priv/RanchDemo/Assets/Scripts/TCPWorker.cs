using UnityEngine;
using System.Collections;
using System.Net.Sockets;
using System.IO;
using System.Threading;
using System.Collections.Generic;
using System;

public class TCPWorker : MonoBehaviour {

	public GameObject prefab;

	public string server = "192.168.0.101";
	public int port = 8080;

	private Dictionary<int, Player> players;

	private Player mainPlayer;

	private byte session;

	void Awake() {
//		DontDestroyOnLoad(this);
	}
	
	// Use this for initialization
	void Start() {
		mainPlayer = NewPlayer ();

		players = new Dictionary<int, Player> ();

		startServer(server, port);

		var bytes = new byte[1] {GameMessage.W_CONNECT};
		send (new Message (bytes));

		StartCoroutine (SendStatus());
	}

	private Player NewPlayer()
	{
		var player = Instantiate (prefab);
		player.transform.parent = transform;
		var result = player.GetComponent<Player> ();
		result.Visible (false);
		return result;
	}

	void OnDisable()
	{
		closeServer ();
		Debug.Log ("Close server");
	}

	IEnumerator SendStatus()
	{
		while (writer != null)
		{
			if (session > 0)
			{
				var position = mainPlayer.GetPosition();
				var x = (int) ((position.x / 5f * 128f) + 127);
				var y = (int) ((position.y / 5f * 128f) + 127);
				var z = (int) ((position.z / 5f * 128f) + 127);
				byte[] bytes = new byte[5];
				bytes[0] = GameMessage.W_POSITION;
				bytes[1] = session;
				bytes[2] = (byte) x;
				bytes[3] = (byte) y;
				bytes[4] = (byte) z;
				send(new Message(bytes));
			}
			yield return new WaitForSeconds(0.1f);
		}
	}

	// Update is called once per frame
	void Update() {
		var msg = processMessage();
		if (msg != null) 
		{
			// do some processing here, like update the player state
//			string s = msg.content;

//			Debug.Log("Received [0] " + msg.content[0]);
//			Debug.Log("Received [1] " + msg.content[1]);
//			Debug.Log("Received [2] " + msg.content[2]);

			var type = msg.content[0];
			var sess = msg.content[1];
			Debug.Log("{Received msg : " + type + "}");
			switch (type)
			{
				case GameMessage.R_INIT:
					Init(sess, msg.content);
					break;
				case GameMessage.R_POSITION:
					UpdatePlayer(sess, msg.content);
					break;
				case GameMessage.R_REMOVE:
					RemovePlayer(sess);
					break;
			}
		}
	}

	private void Init(byte session, byte[] data)
	{
		Debug.Log ("[Init]");
		this.session = session;
		var color = new Color(data[2]/255f, data[3]/255f, data[4]/255f);
		mainPlayer.SetColor(color);
		var x = 5f * (data[5] - 127) / 128f;
		var y = 5;
		var z = 5f * (data[7] - 127) / 128f;

		mainPlayer.SetPosition (new Vector3 (x, y, z));

		Debug.Log ("color " + color);
		Debug.Log ("position " + new Vector3(x, y, z));
		mainPlayer.Visible (true);
	}

	private void UpdatePlayer(byte session, byte[] data)
	{
		var x = 5f * (data[2] - 127) / 128f;
		var y = 5f * (data[3] - 127) / 128f;
		var z = 5f * (data[4] - 127) / 128f;

		Player player;
		if (!players.TryGetValue(session, out player)) 
		{
			player = NewPlayer();
			player.SetColor(Color.grey);
			player.Visible(true);
			players.Add(session, player);
		}
		player.SetPosition (new Vector3 (x, y, z));
	}

	private void RemovePlayer(byte session)
	{
		Player player;
		if (players.TryGetValue(session, out player))
		{
			player.Visible(false);
			Destroy(player.gameObject);
		}
		players.Remove (session);
	}

	private static TcpClient client = null;
	private static BinaryReader reader = null;
	private static BinaryWriter writer = null;
	private static Thread networkThread = null;
	private static Queue<Message> messageQueue = new Queue<Message>();

	private static void addItemToQueue(Message item) {
		lock(messageQueue) {
			messageQueue.Enqueue(item);
		}
	}
	
	private static Message getItemFromQueue() {
		lock(messageQueue) {
			if (messageQueue.Count > 0) {
				return messageQueue.Dequeue();
			} else {
				return null;
			}
		}
	}
	
	private static Message processMessage() {
		Message msg = getItemFromQueue();
		if (msg != null) {
		}
		return msg;
	}
	
	private static void startServer(string server, int port) {
		if (networkThread == null) {
			connect(server, port);
			networkThread = new Thread(() => {
				while (reader != null) {
					Message msg = Message.ReadFromStream(reader);
					addItemToQueue(msg);
				}
				lock(networkThread) {
					networkThread = null;
				}
			});
			networkThread.Start();
		}
	}
	
	private static void connect(string server, int port) {
		if (client == null) {
			client = new TcpClient(server, port);
			Stream stream = client.GetStream();
			reader = new BinaryReader(stream);
			writer = new BinaryWriter(stream);
		}
	}

	private static void closeServer()
	{
		client.Close ();
		reader = null;
		writer = null;
	}
	
	public static void send(Message msg) {
		msg.WriteToStream(writer);
		writer.Flush();
	}

}
