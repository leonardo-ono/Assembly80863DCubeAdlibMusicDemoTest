
/**
 *
 * @author leo
 */
public class JavaMidiNoteFrequencyAlgorithm {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        test3();
    }

    private static void test2() {
        //http://www.inspiredacoustics.com/en/MIDI_note_numbers_and_center_frequencies
        for (int noteNumber = 0; noteNumber < 128; noteNumber++) {
            double frequency = 440 * Math.pow(2, (noteNumber - 69) / 12.0);
            int frequencyInt = (int) (1193180 / frequency);
            
            String frequencyHex = "0000" + Integer.toHexString(frequencyInt);
            frequencyHex = frequencyHex.substring(frequencyHex.length() - 4, frequencyHex.length());
            
            if (noteNumber % 8 == 0) {
                System.out.print("\r\n\tdb ");
            }
            
            System.out.print("0" + frequencyHex.subSequence(2, 4) + "h, 0" + frequencyHex.substring(0, 2) + "h" + ((noteNumber % 8 != 7) ? ", " : ""));
        }
        
        System.out.println("");
    }
    
    private static void test() {
        //http://www.inspiredacoustics.com/en/MIDI_note_numbers_and_center_frequencies
        for (int noteNumber = 0; noteNumber < 128; noteNumber++) {
            double frequency = 440 * Math.pow(2, (noteNumber - 69) / 12.0);
            int frequencyInt = (int) frequency;
            
            // convert to adlib f_num
            int block = 5;
            double f_num = frequency * Math.pow(2, (20 - block)) / 49716;
            String f_num_hex = "0000" + Integer.toHexString((int) f_num);
            f_num_hex = f_num_hex.substring(f_num_hex.length() - 4, f_num_hex.length());
            System.out.println(noteNumber + " = " + frequency + " ---> f_num_hex = " + f_num_hex);
            //if (noteNumber % 8 == 0) {
            //    System.out.print("\r\n\t\t\tdb ");
            //}
            
            //System.out.print("0" + f_num_hex.subSequence(2, 4) + "h, 0" + f_num_hex.substring(0, 2) + "h" + ((noteNumber % 8 != 7) ? ", " : ""));
            
        }

        System.out.println("");
        
    }
    
    private static void test3() {
// http://www.shikadi.net/moddingwiki/OPL_chip#A0-A8:_Frequency_Number        
//Block
//0	0.047 Hz	48.503 Hz	0.048 Hz
//1	0.094 Hz	97.006 Hz	0.095 Hz
//2	0.189 Hz	194.013 Hz	0.190 Hz
//3	0.379 Hz	388.026 Hz	0.379 Hz
//4	0.758 Hz	776.053 Hz	0.759 Hz
//5	1.517 Hz	1,552.107 Hz	1.517 Hz
//6	3.034 Hz	3,104.215 Hz	3.034 Hz
//7	6.068 Hz	6,208.431 Hz	6.069 Hz
        for (int noteNumber = 0; noteNumber < 128; noteNumber++) {
            double frequency = 440 * Math.pow(2, (noteNumber - 69) / 12.0);
            
            boolean clipMax = false;
            int block = 0;
            
            if (frequency <= 48.503) {
                block = 0;
            }
            else if (frequency <= 97.006) {
                block = 1;
            }
            else if (frequency <= 194.013) {
                block = 2;
            }
            else if (frequency <= 388.026) {
                block = 3;
            }
            else if (frequency <= 776.053) {
                block = 4;
            }
            else if (frequency <= 1552.107) {
                block = 5;
            }
            else if (frequency <= 3104.215) {
                block = 6;
            }
            else if (frequency <= 6208.431) {
                block = 7;
                
            }
            else {
                //throw new RuntimeException("invalid frequency !");
                //System.out.println("unsupported frequency " + frequency + " !");
                //System.exit(-1);
                block = 7;
                clipMax = true;
            }
         
            double f_num = frequency * Math.pow(2, (20 - block)) / 49716;
            if (clipMax) {
                f_num = 1023;
            }
            String f_num_hex = "0000" + Integer.toHexString((int) f_num);
            f_num_hex = f_num_hex.substring(f_num_hex.length() - 4, f_num_hex.length());
            //System.out.println(noteNumber + " = " + frequency + " ---> block: " + block + " f_num_hex = " + f_num_hex);
            //System.out.println(noteNumber + " = " + frequency);
            
            String block_hex = "000" + Integer.toHexString(block);
            block_hex = block_hex.substring(block_hex.length() - 3, block_hex.length());

            if (noteNumber % 4 == 0) {
                System.out.print("\r\n\t\t\tdb ");
            }
            System.out.print(block_hex + "h, 000h, 0" + f_num_hex.subSequence(2, 4) + "h, 0" + f_num_hex.substring(0, 2) + "h" + ((noteNumber % 4 != 3) ? ", " : ""));
            
        }
        System.out.println("");
    }
}