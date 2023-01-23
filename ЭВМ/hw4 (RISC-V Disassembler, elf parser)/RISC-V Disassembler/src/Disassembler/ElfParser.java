package Disassembler;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ElfParser {
    byte[] bytes;
    public ElfParser(String fileName) throws IOException {
        try (BufferedInputStream file = new BufferedInputStream(new FileInputStream(fileName))) {
            bytes = file.readAllBytes();
        }
        checkElf();
    }

    private byte read(int offset) {
        return read(offset, 1)[0];
    }

    private byte[] read(int offset, int size) {
        if (offset + size > bytes.length) {
            throw error("Unexpected end of file");
        }
        byte[] chunk = new byte[size];
        System.arraycopy(bytes, offset, chunk, 0,  size);
        return chunk;
    }

    public byte[] getCode() {
        int[] codeLocation = findSectionText();
        return read(codeLocation[0], codeLocation[1]);
    }

    public List<String[]> getSymbolTable() {
        List<String[]> symbols = new ArrayList<>();
        int[] pos = findSymbolTable();
        int symTableOffset = pos[0];
        int symTableSize = pos[1];
        int symTableEntrySize = pos[2];
        for (int i = symTableOffset; i < symTableOffset + symTableSize; i += symTableEntrySize) {
            symbols.add(getSymbol(i));
        }
        return symbols;
    }

    public void dumpSymbolTable() {
        List<String[]> symbols = getSymbolTable();
        String header = String.format("%s %-10s\t%-5s %-8s %-8s %-9s %-6s %s",
                "Symbol", "Value", "Size", "Type", "Bind", "Vis", "Index", "Name");
        System.out.println(header);
        for (int i = 0; i < symbols.size(); i++) {
            String[] symbol = symbols.get(i);
            String out = String.format("[%4d] %-10s\t%-5s %-8s %-8s %-9s %-6s %s",
                    i, "0x" + symbol[0], symbol[1], symbol[2], symbol[3], symbol[4], symbol[5], symbol[6]);
            System.out.println(out);
        }
    }

    public void dumpSymbolTable(Writer file) throws IOException {
        List<String[]> symbols = getSymbolTable();
        String header = String.format("%s %-10s\t%-5s %-8s %-8s %-9s %-6s %s",
                "Symbol", "Value", "Size", "Type", "Bind", "Vis", "Index", "Name");
        file.write(header + '\n');
        for (int i = 0; i < symbols.size(); i++) {
            String[] symbol = symbols.get(i);
            String out = String.format("[%4d] %-10s\t%-5s %-8s %-8s %-9s %-6s %s",
                    i, "0x" + symbol[0], symbol[1], symbol[2], symbol[3], symbol[4], symbol[5], symbol[6]);
            file.write(out + '\n');
        }
    }

    private String[] getSymbol(int offset) {
        int indexInStringTable = parseBytesToInt(read(offset, 4));
        String name = getSymbolName(indexInStringTable);
        String value = Integer.toHexString(parseBytesToInt(read(offset + 4, 4)));
        String size = String.valueOf(parseBytesToInt(read(offset + 8, 4)));
        String bind = getSymbolBind(read(offset + 12) >>> 4);
        String type = getSymbolType(read(offset + 12) & 0x0F);
        String vis = getSymbolVis(read(offset + 13));
        String index = getSymbolIndex(parseBytesToInt(read(offset + 14, 2)));
        return new String[] { value, size, type, bind, vis, index, name };
    }

    public HashMap<Integer, String> getLabels() {
        int index = getSectionTextIndex();
        List<String[]> symbols = getSymbolTable();
        HashMap<Integer, String> labels = new HashMap<>();
        for (String[] symbol : symbols) {
            if (symbol[5].equals(String.valueOf(index))) {
                labels.put(translateVirtualAddressToOffsetInSectionText(Integer.parseInt(symbol[0], 16)), symbol[6]);
            }
        }
        return labels;
    }

    private String getSymbolName(int indexInStringTable) {
        int stringTableOffset = getStringTableOffset();
        StringBuilder name = new StringBuilder();

        byte c = read(stringTableOffset + indexInStringTable);
        int i = 1;
        while (c != 0x00) {
            name.append((char) c);
            c = read(stringTableOffset + indexInStringTable + i);
            i++;
        }

        return name.toString();
    }

    private int[] findSymbolTable() {
        int offset = getSectionHeaderOffset(new int[] { 0x2E, 0X73, 0x79, 0x6D, 0x74, 0x61, 0x62, 0x00 });
        int symTableOffset = parseBytesToInt(read(offset + 16, 4));
        int symTableSize = parseBytesToInt(read(offset + 20, 4));
        int symTableEntrySize = parseBytesToInt(read(offset + 36, 4));
        return new int[] { symTableOffset, symTableSize, symTableEntrySize };
    }

    private int[] findSectionText() {
        int offset = getSectionHeaderOffset(new int[] { 0x2E, 0x74, 0x65, 0x78, 0x74, 0x00 });
        int sectionTextOffset = parseBytesToInt(read(offset + 16, 4));
        int sectionTextSize = parseBytesToInt(read(offset + 20, 4));
        return new int[] { sectionTextOffset, sectionTextSize };
    }

    private int getSectionTextIndex() {
        return getSectionIndex(new int[] { 0x2E, 0x74, 0x65, 0x78, 0x74, 0x00 });
    }

    private int getSectionIndex(int[] name) {
        int shoff = getShoff(); //Section header offset
        int shnum = getShnum(); //Section headers number
        int shentsize = getShentsize(); //Section header entry size
        int stringTableOffset = getSectionHeaderStringTableOffset();
        for (int i = 0; i < shnum; i++) {
            if (isSection(shoff + i * shentsize, stringTableOffset, name)) {
                return i;
            }
        }
        throw error("Section header not found");
    }

    private int getSectionHeaderOffset(int[] name) {
        int shoff = getShoff(); //Section header offset
        int shnum = getShnum(); //Section headers number
        int shentsize = getShentsize(); //Section header entry size
        int sectionHeaderStringTableOffset = getSectionHeaderStringTableOffset();
        for (int i = 0; i < shnum; i++) {
            if (isSection(shoff + i * shentsize, sectionHeaderStringTableOffset, name)) {
                return shoff + i * shentsize;
            }
        }
        throw error("Section header not found");
    }

    private int getStringTableOffset() {
        int sectionHeaderOffset = getSectionHeaderOffset(new int[] { 0x2E, 0x73, 0x74, 0x72, 0x74, 0x61, 0x62, 0x00 });
        int stringTableOffset = parseBytesToInt(read(sectionHeaderOffset + 16, 4));
        return stringTableOffset;
    }

    private int getSectionHeaderStringTableOffset() {
        int shtrndx = getShtrndx();
        int shoff = getShoff();
        int shentsize = getShentsize();
        return parseBytesToInt(read(shoff + shtrndx * shentsize + 16, 4));
    }

    private boolean isSection(int offset, int stringTableOffset, int[] name) {
        int offsetInStringTable = parseBytesToInt(read(offset, 4));
        return check(stringTableOffset + offsetInStringTable, name);
    }

    private String getSymbolIndex(int index) {
        switch(index) {
            case (0):
                return "UNDEF";
            case (0xff00):
                return "LOPROC";
            case (0xff1f):
                return "HIPROC";
            case (0xff20):
                return "LOOS";
            case (0xff3f):
                return "HIOS";
            case (0xfff1):
                return "ABS";
            case (0xfff2):
                return "COMMON";
            case (0xffff):
                return "XINDEX";
            default:
                return String.valueOf(index);
        }
    }

    private String getSymbolVis(int type) {
        switch(type) {
            case (0):
                return "DEFAULT";
            case (1):
                return "INTERNAL";
            case (2):
                return "HIDDEN";
            case (3):
                return "PROTECTED";
            default:
                return "UNKNOWN";
        }
    }

    private String getSymbolBind(int type) {
        switch(type) {
            case (0):
                return "LOCAL";
            case (1):
                return "GLOBAL";
            case (2):
                return "WEAK";
            case (10):
                return "LOOS";
            case (12):
                return "HIOS";
            case (13):
                return "LOPROC";
            case (15):
                return "HIPROC";
            default:
                return "UNKNOWN";
        }
    }

    private String getSymbolType(int type) {
        switch(type) {
            case (0):
                return "NOTYPE";
            case (1):
                return "OBJECT";
            case (2):
                return "FUNC";
            case (3):
                return "SECTION";
            case (4):
                return "FILE";
            case (5):
                return "COMMON";
            case (6):
                return "TLS";
            case (10):
                return "LOOS";
            case (12):
                return "HIOS";
            case (13):
                return "LOPROC";
            case (15):
                return "HIPROC";
            default:
                return "UNKNOWN";
        }
    }

    private int getShtrndx() {
        return parseBytesToInt(read(48, 4)) >>> 16;
    }

    private int getShnum() {
        return parseBytesToInt(read(48, 4)) & 0x0000FFFF;
    }

    private int getShoff() {
        return parseBytesToInt(read(32, 4));
    }

    private int getShentsize() {
        return parseBytesToInt(read(44, 4)) >>> 16;
    }

    public int getSectionTextVirtualAddress() {
        int offset = getSectionHeaderOffset(new int[] { 0x2E, 0x74, 0x65, 0x78, 0x74, 0x00 });
        int sectionTextVirtualAddress = parseBytesToInt(read(offset + 12, 4));
        return sectionTextVirtualAddress;
    }

    private int translateVirtualAddressToOffsetInSectionText(int address) {
        return address - getSectionTextVirtualAddress();
    }

    private void checkElf() {
        if (!check(0, new int[] { 0x7F, 0x45, 0x4C, 0x46 })) {
            throw error("Invalid file type: elf expected");
        }
    }

    private boolean check(int offset, int[] expected) {
        for (int i = 0; i < expected.length; i++) {
            if (expected[i] != read(offset + i)) {
                return false;
            }
        }
        return true;
    }

    public static int parseBytesToInt(byte[] bytes) {
        int parsed = bytes[bytes.length - 1] & 0b011111111;
        for (int i = bytes.length - 2; i >= 0; i--) {
            parsed <<= 8;
            parsed = parsed | (bytes[i] & 0b011111111);
        }
        return parsed;
    }

    public ParseException error(final String message) {
        return new ParseException(message);
    }
}