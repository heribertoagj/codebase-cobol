      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW86A
      * Data/Hora : 21/Dez/2017 14:36:42
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW86A-CD-FRMUL                                     PIC X(8)
           VALUE 'DCOMW86A'.
       05 DCOMW86A-CAG-BCRIA                                   PIC 9(5).
       05 DCOMW86A-DIGITO-AGEN                                 PIC X(2).
       05 DCOMW86A-NOME-AGEN                                  PIC X(20).
       05 DCOMW86A-CCTA-BCRIA                                 PIC 9(13).
       05 DCOMW86A-DIGITO-CONTA                                PIC X(2).
       05 DCOMW86A-CCNPJ-CPF                                   PIC 9(9).
       05 DCOMW86A-CFLIAL-CNPJ                                 PIC 9(4).
       05 DCOMW86A-CCTRL-CNPJ-CPF                              PIC 9(2).
       05 DCOMW86A-OPERACAO                                   PIC 9(13).
       05 DCOMW86A-DINIC-OPER                                 PIC X(10).
       05 DCOMW86A-VOPER-DESC                                 PIC 9(17).
       05 DCOMW86A-IPSSOA-DESC                                PIC X(40).
       05 DCOMW86A-CONTR-COD-BARR                              PIC 9(9).
       05 DCOMW86A-NUM-COD-BARR                                PIC 9(1).
       05 DCOMW86A-MM-AAAA-BARR                                PIC 9(6).
       05 DCOMW86A-TIPO-VIA                                    PIC 9(1).
       05 DCOMW86A-VTOT-IOF-CHQ                               PIC 9(15).
       05 DCOMW86A-VTOT-JUR-CHQ                               PIC 9(15).
       05 DCOMW86A-VTOT-LIQ-CHQ                               PIC 9(17).
       05 DCOMW86A-VSOMATOT-CHQ                               PIC 9(17).
       05 DCOMW86A-NOCOR                                       PIC 9(2).
       05 DCOMW86A-OCORRENCIA
           OCCURS 1 TO 50 TIMES
           DEPENDING ON DCOMW86A-NOCOR.
          10 DCOMW86A-CD-LANC                                   PIC X(8)
              VALUE 'DC05L'.
          10 DCOMW86A-CBCO-CHQ                                 PIC 9(3).
          10 DCOMW86A-AGEN-CHQ                                 PIC 9(5).
          10 DCOMW86A-CTA-CHQ                                 PIC 9(13).
          10 DCOMW86A-CCPFCNPJ-CHQ                             PIC 9(9).
          10 DCOMW86A-CFILIAL-CHQ                              PIC 9(4).
          10 DCOMW86A-CCONT-CHQ                                PIC 9(2).
          10 DCOMW86A-NCHQ                                     PIC 9(7).
          10 DCOMW86A-DT-DEPOS                                PIC X(10).
          10 DCOMW86A-VIOF-CHQ                                PIC 9(15).
          10 DCOMW86A-VJUR-CHQ                                PIC 9(15).
          10 DCOMW86A-VLIQ-CHQ                                PIC 9(15).
          10 DCOMW86A-VTOT-CHQ                                PIC 9(17).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
