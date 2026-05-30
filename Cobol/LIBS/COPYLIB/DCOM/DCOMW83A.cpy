      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW83A
      * Data/Hora : 13/Dez/2017 14:44:08
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW83A-CD-FRMUL                                     PIC X(8)
           VALUE 'DCOMW83A'.
       05 DCOMW83A-CAG-BCRIA                                   PIC 9(5).
       05 DCOMW83A-DIGITO-AGEN                                 PIC X(2).
       05 DCOMW83A-CCTA-BCRIA                                 PIC 9(13).
       05 DCOMW83A-DIGITO-CONTA                                PIC X(2).
       05 DCOMW83A-CCNPJ-CPF                                   PIC 9(9).
       05 DCOMW83A-CFLIAL-CNPJ                                 PIC 9(4).
       05 DCOMW83A-CCTRL-CNPJ-CPF                              PIC 9(2).
       05 DCOMW83A-OPERACAO                                   PIC 9(13).
       05 DCOMW83A-DINIC-OPER                                 PIC X(10).
       05 DCOMW83A-VOPER-DESC                                 PIC 9(17).
       05 DCOMW83A-NOME-AGEN                                  PIC X(20).
       05 DCOMW83A-IPSSOA-DESC                                PIC X(40).
       05 DCOMW83A-VLIQ-OPER                                  PIC 9(15).
       05 DCOMW83A-PTX-JURO-MES                               PIC 9(13).
       05 DCOMW83A-PTX-JURO-ANO                               PIC 9(13).
       05 DCOMW83A-CCART-LIM                                   PIC X(5).
       05 DCOMW83A-DPRIM-VCTO                                 PIC X(10).
       05 DCOMW83A-DVCTO-FNAL                                 PIC X(10).
       05 DCOMW83A-VNOTA-PROMIS                               PIC 9(15).
       05 DCOMW83A-VIOF-OPER                                  PIC 9(15).
       05 DCOMW83A-VTAC-OPER                                  PIC 9(15).
       05 DCOMW83A-VTARIF-REG                                 PIC 9(17).
       05 DCOMW83A-VTRIBUTOS                                  PIC 9(17).
       05 DCOMW83A-TRIBUTOS                                    PIC 9(5).
       05 DCOMW83A-VSEGUROS                                   PIC 9(17).
       05 DCOMW83A-PSEGUROS                                    PIC 9(5).
       05 DCOMW83A-VTARIFA-CET                                PIC 9(17).
       05 DCOMW83A-PTARIFA-CET                                 PIC 9(5).
       05 DCOMW83A-VPGTOSERV                                  PIC 9(17).
       05 DCOMW83A-PPGTOSERV                                   PIC 9(5).
       05 DCOMW83A-VREGISTROS                                 PIC 9(17).
       05 DCOMW83A-PREGISTROS                                  PIC 9(5).
       05 DCOMW83A-VCET-TOTAL                                 PIC 9(17).
       05 DCOMW83A-PCET-TOTAL                                  PIC 9(5).
       05 DCOMW83A-VCET-MES                                   PIC 9(15).
       05 DCOMW83A-VCET-ANO                                   PIC 9(15).
       05 DCOMW83A-VTOT-LIB                                   PIC 9(17).
       05 DCOMW83A-TOT-LIB                                     PIC 9(5).
       05 DCOMW83A-CONTR-COD-BARR                              PIC 9(9).
       05 DCOMW83A-NUM-COD-BARR                                PIC 9(1).
       05 DCOMW83A-MM-AAAA-BARR                                PIC 9(6).
       05 DCOMW83A-TIPO-VIA                                    PIC 9(1).
       05 DCOMW83A-NOCOR                                       PIC 9(2).
       05 DCOMW83A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW83A-NOCOR.
          10 DCOMW83A-CD-LANC                                   PIC X(8)
              VALUE 'DC02L'.
          10 DCOMW83A-NOME-AVAL                               PIC X(40).
          10 DCOMW83A-CPFPJ-AVAL                              PIC X(19).
          10 DCOMW83A-IPSSOA-CONJG                            PIC X(40).
          10 DCOMW83A-CPFPJ-CONJG                             PIC X(19).
          10 DCOMW83A-NOME-TEST                               PIC X(40).
          10 DCOMW83A-CPFPJ-TEST                              PIC X(19).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
