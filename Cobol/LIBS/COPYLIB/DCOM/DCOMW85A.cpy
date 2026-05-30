      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW85A
      * Data/Hora : 21/Dez/2017 14:36:59
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW85A-CD-FRMUL                                     PIC X(8)
           VALUE 'DCOMW85A'.
       05 DCOMW85A-CAG-BCRIA                                   PIC 9(5).
       05 DCOMW85A-DIGITO-AGEN                                 PIC X(2).
       05 DCOMW85A-NOME-AGEN                                  PIC X(20).
       05 DCOMW85A-CCTA-BCRIA                                 PIC 9(13).
       05 DCOMW85A-DIGITO-CONTA                                PIC X(2).
       05 DCOMW85A-CCNPJ-CPF                                   PIC 9(9).
       05 DCOMW85A-CFLIAL-CNPJ                                 PIC 9(4).
       05 DCOMW85A-CCTRL-CNPJ-CPF                              PIC 9(2).
       05 DCOMW85A-OPERACAO                                   PIC 9(13).
       05 DCOMW85A-DINIC-OPER                                 PIC X(10).
       05 DCOMW85A-VOPER-DESC                                 PIC 9(17).
       05 DCOMW85A-IPSSOA-DESC                                PIC X(40).
       05 DCOMW85A-VTAC-OPER-DESC                             PIC 9(15).
       05 DCOMW85A-VTAR-OPER                                  PIC 9(15).
       05 DCOMW85A-VIOF-OPER-DESC                             PIC 9(15).
       05 DCOMW85A-VTOT-JUROS                                 PIC 9(15).
       05 DCOMW85A-CONTR-COD-BARR                              PIC 9(9).
       05 DCOMW85A-NUM-COD-BARR                                PIC 9(1).
       05 DCOMW85A-MM-AAAA-BARR                                PIC 9(6).
       05 DCOMW85A-TIPO-VIA                                    PIC 9(1).
       05 DCOMW85A-QTDE-TITLO                                  PIC 9(5).
       05 DCOMW85A-VTOT-IOF                                   PIC 9(17).
       05 DCOMW85A-VTOT-JUR                                   PIC 9(17).
       05 DCOMW85A-VTOT-TITLO                                 PIC 9(17).
       05 DCOMW85A-VTOT-LIQUIDO                               PIC 9(17).
       05 DCOMW85A-NOCOR                                       PIC 9(2).
       05 DCOMW85A-OCORRENCIA
           OCCURS 1 TO 50 TIMES
           DEPENDING ON DCOMW85A-NOCOR.
          10 DCOMW85A-CD-LANC                                   PIC X(8)
              VALUE 'DC04L'.
          10 DCOMW85A-CTITLO-COBR-BCO                         PIC X(20).
          10 DCOMW85A-NOME-SACADO                             PIC X(40).
          10 DCOMW85A-CNPJ-SAC                                 PIC 9(9).
          10 DCOMW85A-FILIAL-SAC                               PIC 9(4).
          10 DCOMW85A-CTRL-SAC                                 PIC 9(2).
          10 DCOMW85A-DVCTO-TITLO                             PIC X(10).
          10 DCOMW85A-VALOR-IOF                               PIC 9(15).
          10 DCOMW85A-VALOR-JUROS                             PIC 9(15).
          10 DCOMW85A-VTITLO-DESC                             PIC 9(15).
          10 DCOMW85A-VALOR-LIQUIDO                           PIC 9(15).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
