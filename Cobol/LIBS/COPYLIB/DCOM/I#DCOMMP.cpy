      ******************************************************************
      * SISTEMA : DCOM - I#DCOMMP                                      *
      * TAMANHO : 0256 BYTES                                           *
      * ARQUIVO : PRODUCAO MENSAL                                      *
      ******************************************************************
       01  MP-REGISTRO.
      *    --->> CHAVE
  1  5     03  MP-CJUNC-DEPDC               PIC  9(05).
  6 13     03  MP-CCCORR                    PIC  9(13).
 19  9     03  MP-CCNPJ-CPF                 PIC  9(09).
 28  5     03  MP-CFLIAL-CNPJ               PIC  9(05).
 33  2     03  MP-CCTRL-CNPJ                PIC  9(02).
 35  5     03  MP-CPOSTO-SERVC              PIC  9(05).
 40  2     03  MP-CTPO-PAB                  PIC  X(02).
 42  3     03  MP-SEGMENTO-CLIE             PIC  9(03).
 45  3     03  MP-CPRODT                    PIC  9(03).
 48  3     03  MP-CSPROD-DESC               PIC  9(03).
 51  2     03  MP-CINDCD-CONTR-CONVE        PIC  X(02).
 53  9     03  MP-CCONTR-CONVE-DESC         PIC  9(09).
 62  3     03  MP-CVRSAO-CONTR-CONVE        PIC  9(03).
      *    --->> ATRIBUTOS
 65  4     03  MP-DANO-OPER-DESC            PIC  9(04).
 69  9     03  MP-NSEQ-OPER-DESC            PIC  9(09).
 78  3     03  MP-CTPO-GARNTIA-PRINC        PIC  9(03).
 81  3     03  MP-CTPO-GARNTIA-COMPL        PIC  9(03).
 84 10     03  MP-DINIC-OPER-DESC           PIC  X(10).
 94 10     03  MP-DFNAL-OPER-DESC           PIC  X(10).
104  9     03  MP-TX-MENSAL                 PIC  9(03)V9(06).
113  9     03  MP-TX-ANUAL                  PIC  9(03)V9(06).
122  5     03  MP-TOPER-DESC                PIC  9(05).
127  5     03  MP-PRZO-MEDIO-OPER           PIC  9(05).
132 17     03  MP-VOPER-DESC-COML           PIC  9(15)V9(02).
149 17     03  MP-VIOF-OPER-DESC            PIC  9(15)V9(02).
166 17     03  MP-VTAC-OPER-DESC            PIC  9(15)V9(02).
183 17     03  MP-VTARIF-TITLO-OPER-DESC    PIC  9(15)V9(02).
200 57     03  FILLER                       PIC  X(57).
