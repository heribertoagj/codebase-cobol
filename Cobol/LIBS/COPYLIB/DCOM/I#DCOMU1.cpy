      ******************************************************************
      *SISTEMA = NET EMPRESA - XXXXXX                  BOOK = I#DCOMU1 *
      *ARQUIVO = ENTRADAS SUMARIZADO - DCOMB0C5                        *
      *LRECL   = 050 BYTES                             07/08/2007      *
      ******************************************************************
001    01 U1-REGISTRO.
001       05 U1-CHAVE.
001          10 U1-CCORR.
001 02          15 U1-CBCO              PIC  S9(003)V     COMP-3.
003 03          15 U1-CAG-BCRIA         PIC  S9(005)V     COMP-3.
006 07          15 U1-CCTA-BCRIA-CLI    PIC  S9(013)V     COMP-3.
013 02       10 U1-CPRODT               PIC  S9(003)V     COMP-3.
015 02       10 U1-CSPROD-DESC-COML     PIC  S9(003)V     COMP-3.
017 08       10 U1-DENTRD-CONSL-DESC    PIC   9(008).
017          10 FILLER  REDEFINES       U1-DENTRD-CONSL-DESC.
017 04          15 U1-DANOENTRD         PIC   9(004).
021 02          15 U1-DMESENTRD         PIC   9(002).
023 02          15 U1-DDIAENTRD         PIC   9(002).
025 09    05 U1-VPOSIC-ENTRD-CONSL      PIC  S9(015)V99   COMP-3.
034 09    05 U1-QTITLO-ENTRD            PIC  S9(009)V     COMP-3.
043 08    05 FILLER                     PIC   X(012).
