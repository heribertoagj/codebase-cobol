      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW84A
      * Data/Hora : 13/Dez/2017 14:44:25
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW84A-CD-FRMUL                                     PIC X(8)
           VALUE 'DCOMW84A'.
       05 DCOMW84A-CAG-BCRIA                                   PIC 9(5).
       05 DCOMW84A-DIGITO-AGEN                                 PIC X(2).
       05 DCOMW84A-CCTA-BCRIA                                 PIC 9(13).
       05 DCOMW84A-DIGITO-CONTA                                PIC X(2).
       05 DCOMW84A-CCNPJ-CPF                                   PIC 9(9).
       05 DCOMW84A-CFLIAL-CNPJ                                 PIC 9(4).
       05 DCOMW84A-CCTRL-CNPJ-CPF                              PIC 9(2).
       05 DCOMW84A-OPERACAO                                   PIC 9(13).
       05 DCOMW84A-DINIC-OPER                                 PIC X(10).
       05 DCOMW84A-VOPER-DESC                                 PIC 9(17).
       05 DCOMW84A-IPSSOA-DESC                                PIC X(40).
       05 DCOMW84A-DPRIM-VCTO                                 PIC X(10).
       05 DCOMW84A-DVCTO-FNAL                                 PIC X(10).
       05 DCOMW84A-PTX-JURO-MES                               PIC 9(13).
       05 DCOMW84A-PTX-JURO-ANO                               PIC 9(13).
       05 DCOMW84A-VIOF-OPER-DESC                             PIC 9(15).
       05 DCOMW84A-VTAC-OPER-DESC                             PIC 9(15).
       05 DCOMW84A-QTITLO-CHEQ                                 PIC 9(9).
       05 DCOMW84A-VAL-OPER-EXT                              PIC X(200).
       05 DCOMW84A-VTRIBUTOS                                  PIC 9(17).
       05 DCOMW84A-TRIBUTOS                                    PIC 9(5).
       05 DCOMW84A-VSEGUROS                                   PIC 9(17).
       05 DCOMW84A-PSEGUROS                                    PIC 9(5).
       05 DCOMW84A-VTARIFA-CET                                PIC 9(17).
       05 DCOMW84A-PTARIFA-CET                                 PIC 9(5).
       05 DCOMW84A-VPGTOSERV                                  PIC 9(17).
       05 DCOMW84A-PPGTOSERV                                   PIC 9(5).
       05 DCOMW84A-VREGISTROS                                 PIC 9(17).
       05 DCOMW84A-PREGISTROS                                  PIC 9(5).
       05 DCOMW84A-VCET-TOTAL                                 PIC 9(17).
       05 DCOMW84A-PCET-TOTAL                                  PIC 9(5).
       05 DCOMW84A-VCET-MES                                   PIC 9(15).
       05 DCOMW84A-VCET-ANO                                   PIC 9(15).
       05 DCOMW84A-VTOT-LIB                                   PIC 9(17).
       05 DCOMW84A-TOT-LIB                                     PIC 9(5).
       05 DCOMW84A-CONTR-COD-BARR                              PIC 9(9).
       05 DCOMW84A-NUM-COD-BARR                                PIC 9(1).
       05 DCOMW84A-MM-AAAA-BARR                                PIC 9(6).
       05 DCOMW84A-TIPO-VIA                                    PIC 9(1).
       05 DCOMW84A-NOCOR                                       PIC 9(2).
       05 DCOMW84A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW84A-NOCOR.
          10 DCOMW84A-CD-LANC                                   PIC X(8)
              VALUE 'DC03L'.
          10 DCOMW84A-NOME-AVAL                               PIC X(40).
          10 DCOMW84A-CPFPJ-AVAL                              PIC X(19).
          10 DCOMW84A-NOME-TEST                               PIC X(40).
          10 DCOMW84A-CPFPJ-TEST                              PIC X(19).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
