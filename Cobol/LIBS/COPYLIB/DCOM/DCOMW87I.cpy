      ******************************************************************
      * NOME BOOK : DCOMW87I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 21/11/2017                                         *
      * AUTOR     : DANILO CHIDIAK AMAZONAS                            *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 00564 BYTES                                        *
      ******************************************************************
       05 DCOMW87I-HEADER.
         10 DCOMW87I-COD-LAYOUT            PIC X(08)  VALUE 'DCOMW87I'.
         10 DCOMW87I-TAM-LAYOUT            PIC 9(05)  VALUE 00564.
       05 DCOMW87I-REGISTRO.
         10 DCOMW87I-BLOCO-ENTRADA.
           15 DCOMW87I-E-NUM-OPER          PIC 9(13).
           15 DCOMW87I-E-USUARIO           PIC X(09).
         10 DCOMW87I-BLOCO-SAIDA.
           15 DCOMW87I-S-CD-FRMUL          PIC X(08)  VALUE 'DCOMW87A'.
           15 DCOMW87I-S-CAG-BCRIA         PIC 9(05).
           15 DCOMW87I-S-DIGITO-AGEN       PIC X(02).
           15 DCOMW87I-S-NOME-AGEN         PIC X(20).
           15 DCOMW87I-S-CCTA-BCRIA        PIC 9(13).
           15 DCOMW87I-S-DIGITO-CONTA      PIC X(02).
           15 DCOMW87I-S-CCNPJ-CPF         PIC 9(09).
           15 DCOMW87I-S-CFLIAL-CNPJ       PIC 9(04).
           15 DCOMW87I-S-CCTRL-CNPJ-CPF    PIC 9(02).
           15 DCOMW87I-S-OPERACAO          PIC 9(13).
           15 DCOMW87I-S-DINIC-OPER        PIC X(10).
           15 DCOMW87I-S-VOPER-DESC        PIC 9(15)V99.
           15 DCOMW87I-S-IPSSOA-DESC       PIC X(40).
           15 DCOMW87I-S-ENDER             PIC X(40).
           15 DCOMW87I-S-ICIDADE           PIC X(20).
           15 DCOMW87I-S-CEP               PIC 9(08).
           15 DCOMW87I-S-SGLUF             PIC X(02).
           15 DCOMW87I-S-NOCOR             PIC 9(02).
           15 DCOMW87I-S-OCORRENCIA        OCCURS 1 TO 4 TIMES
                                           DEPENDING ON
                                           DCOMW87I-S-NOCOR.
             20 DCOMW87I-S-CD-LANC         PIC X(08)  VALUE 'DC007L'.
             20 DCOMW87I-S-ITESTEMUNHA     PIC X(40).
             20 DCOMW87I-S-CCPFCNPJ-TEST   PIC 9(09).
             20 DCOMW87I-S-CFILIAL-TEST    PIC 9(04).
             20 DCOMW87I-S-CCONT-TEST      PIC 9(02).
             20 DCOMW87I-S-CRGTEST         PIC X(15).
