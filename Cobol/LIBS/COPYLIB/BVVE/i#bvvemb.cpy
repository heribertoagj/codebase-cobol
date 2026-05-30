      * ****************************************************** *
      *   INC *** I#BVVEMB   - ARQUIVO PARA SISTEMA TMNF       *
      *                        HEADER.                         *
      *                        TAMANHO = 32000 BYTES           *
      * ****************************************************** *
      *   OBS.: LAYOUT PADRAO DE HEADER E PREFIXO DE DETALHE   *
      * ****************************************************** *
       01       REG-BVVEMB.
         03      BVVEMB-HEADER.
           05    BVVEMB-NOME-ARQUIVO        PIC X(050).
           05    BVVEMB-NUM-CTRL-EMIS       PIC X(020).
           05    BVVEMB-NUM-CTRL-DEST-OR    PIC X(020).
           05    BVVEMB-ISPB-EMISSOR        PIC X(008).
           05    BVVEMB-ISPB-DESTINAT       PIC X(008).
           05    BVVEMB-DT-HR-DDA           PIC X(026).
           05    BVVEMB-SIT-REQ-DDA         PIC X(002).
           05    BVVEMB-INDR-FLAG-FIM       PIC X(001).
           05    BVVEMB-DT-MOVTO            PIC 9(008).
           05    FILLER                     PIC X(31857).
         03      BVVEMB-DETALHE-TMNF REDEFINES BVVEMB-HEADER.
           05    BVVEMB-SEQ-REGISTRO        PIC 9(001).
           05    BVVEMB-IDE-CONTINUIDADE    PIC X(001).
           05    BVVEMB-REGISTRO-DDA        PIC X(31998).

