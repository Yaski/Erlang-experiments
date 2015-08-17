using UnityEngine;
using System.Collections;

[RequireComponent(typeof(Rigidbody))]
public class Player : MonoBehaviour {

	private Rigidbody body;

	private void Awake()
	{
		body = GetComponent<Rigidbody> ();
	}

	public void Visible(bool value)
	{
		GetComponent<MeshRenderer> ().enabled = value;
	}

	public void SetColor(Color value)
	{
		Material material = GetComponent<MeshRenderer> ().material;
		material.SetColor ("_Color", value);
	}

	public void SetPosition(Vector3 value)
	{
		body.position = value;
	}

	public Vector3 GetPosition()
	{
		return body.position;
	}

	// Update is called once per frame
	private void Update () {
	
	}

}
