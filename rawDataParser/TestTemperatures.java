public enum TestTemperatures {
	HOT(280), COLD(-65), ROOM_TEMP(80);
	private int value;
	
	private TestTemperatures(int value){
		this.value = value;
	}
};