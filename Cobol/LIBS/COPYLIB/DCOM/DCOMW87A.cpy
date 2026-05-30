      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW87A
      * Data/Hora : 13/Dez/2017 14:45:30
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW87A-CD-FRMUL                                     PIC X(8)
           VALUE 'DCOMW87A'.
       05 DCOMW87A-CAG-BCRIA                                   PIC 9(5).
       05 DCOMW87A-DIGITO-AGEN                                 PIC X(2).
       05 DCOMW87A-NOME-AGEN                                  PIC X(20).
       05 DCOMW87A-CCTA-BCRIA                                 PIC 9(13).
       05 DCOMW87A-DIGITO-CONTA                                PIC X(2).
       05 DCOMW87A-CCNPJ-CPF                                   PIC 9(9).
       05 DCOMW87A-CFLIAL-CNPJ                                 PIC 9(4).
       05 DCOMW87A-CCTRL-CNPJ-CPF                              PIC 9(2).
       05 DCOMW87A-OPERACAO                                   PIC 9(13).
       05 DCOMW87A-DINIC-OPER                                 PIC X(10).
       05 DCOMW87A-VOPER-DESC                                 PIC 9(17).
       05 DCOMW87A-IPSSOA-DESC                                PIC X(40).
       05 DCOMW87A-ENDER                                      PIC X(40).
       05 DCOMW87A-ICIDADE                                    PIC X(20).
       05 DCOMW87A-CEP                                         PIC 9(8).
       05 DCOMW87A-SGLUF                                       PIC X(2).
       05 DCOMW87A-CONTR-COD-BARR                              PIC 9(9).
       05 DCOMW87A-NUM-COD-BARR                                PIC 9(1).
       05 DCOMW87A-MM-AAAA-BARR                                PIC 9(6).
       05 DCOMW87A-TIPO-VIA                                    PIC 9(1).
       05 DCOMW87A-NOCOR                                       PIC 9(2).
       05 DCOMW87A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW87A-NOCOR.
          10 DCOMW87A-CD-LANC                                   PIC X(8)
              VALUE 'DC07L'.
          10 DCOMW87A-ITESTEMUNHA                             PIC X(40).
          10 DCOMW87A-CPFPJ-TEST                              PIC X(19).
          10 DCOMW87A-CRGTEST                                 PIC X(15).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
