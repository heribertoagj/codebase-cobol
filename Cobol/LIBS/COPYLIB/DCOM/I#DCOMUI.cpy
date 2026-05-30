      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMUI       *
      * ARQUIVO = GESTAO DE TITULOS/CHEQUES EM ALERTA ENVIADOS PARA    *
      *           CANAIS DIGITAIS                                      *
      * LRECL   = 400 BYTES                                            *
      *----------------------------------------------------------------*
      * CAG-BCRIA          -  AGENCIA
      * CCTA-BCRIA-CLI     -  CONTA
      * CCNPJ-CPF          -  CNPJ/CPF
      * NMCLI              -  NOME DO CLIENTE
      * CPRODT             -  PRODUTO
      * IPRODT             -  DESCRICAO DO PRODUTO
      * CSPROD-DESC-COML   -  SUBPRODUTO
      * ISPROD             -  DESCRICAO DO SUBPRODUTO
      * DATA-VENC-TIT      -  DATA VENCIMENTO DO TITULO
      * VLIM-DESC          -  VALOR DO TITULO OU CHEQUE
      * DATA-CARGA-DCOM    -  DATA DA CARGA NO MENU DCOM
      * MOTIV-GATI         -  CODIGO MOTIVO GATI
      * RESTRICAO-GATI     -  DESCRICAO MOTIVO GATI
      * DATA-ENV-TITULO    -  DATA QUE O GERENTE COMANDOU ENVIO DO
      *                       TITULO PARA OS CANAIS NET EMPRESA/MOBILE
      * DESC-TIT-CHEQ      -  MARCA SE O CLIENTE DESCONTOU O
      *                       TITULO/CHEQUE | 'S' OU 'N'
      * CBCO-TIT           -  PARA DUPLICATA: BANCO DO TITULO
      * PRODT-TIT          -  PARA DUPLICATA: PRODUTO DO TITULO
      * CNEGOC-TIT         -  PARA DUPLICATA: NEGOCIACAO DO TITULO
      * CTITLO-TIT         -  PARA DUPLICATA: NUMERO DO TITULO
      * CBCO-CHEQ          -  PARA CHEQUE: BANCO DO CHEQUE
      * AGE-CHEQ           -  PARA CHEQUE: AGENCIA DO CHEQUE
      * CTA-CHEQ           -  PARA CHEQUE: CONTA DO CHEQUE
      * CHE-CHEQ           -  PARA CHEQUE: NUMERO DO CHEQUE
      * CUSUAR-AUTOR       -  CODIGO FUNCIONAL DE QUEM EFETUOU A
      *                       AUTORIZACAO
      *----------------------------------------------------------------*
       01 UI-REGISTRO.
          05 UI-CAG-BCRIA                        PIC  9(005).
          05 UI-FILLER1                          PIC  X(001)  VALUE ';'.
          05 UI-CCTA-BCRIA-CLI                   PIC  9(013).
          05 UI-FILLER2                          PIC  X(001)  VALUE ';'.
          05 UI-CCNPJ-CPF                        PIC  9(015).
          05 UI-FILLER3                          PIC  X(001)  VALUE ';'.
          05 UI-NMCLI                            PIC  X(040).
          05 UI-FILLER4                          PIC  X(001)  VALUE ';'.
          05 UI-CPRODT                           PIC  9(003).
          05 UI-FILLER5                          PIC  X(001)  VALUE ';'.
          05 UI-IPRODT                           PIC  X(030).
          05 UI-FILLER6                          PIC  X(001)  VALUE ';'.
          05 UI-CSPROD-DESC-COML                 PIC  9(003).
          05 UI-FILLER7                          PIC  X(001)  VALUE ';'.
          05 UI-ISPROD                           PIC  X(030).
          05 UI-FILLER8                          PIC  X(001)  VALUE ';'.
          05 UI-DATA-VENC-TIT                    PIC  X(010).
          05 UI-FILLER9                          PIC  X(001)  VALUE ';'.
          05 UI-VLIM-DESC                        PIC  9(017).
          05 UI-FILLER10                         PIC  X(001)  VALUE ';'.
          05 UI-DATA-CARGA-DCOM                  PIC  X(010).
          05 UI-FILLER11                         PIC  X(001)  VALUE ';'.
          05 UI-MOTIV-GATI                       PIC  9(005).
          05 UI-FILLER12                         PIC  X(001)  VALUE ';'.
          05 UI-RESTRICAO-GATI                   PIC  X(050).
          05 UI-FILLER13                         PIC  X(001)  VALUE ';'.
          05 UI-DATA-ENV-TITULO                  PIC  X(010).
          05 UI-FILLER14                         PIC  X(001)  VALUE ';'.
          05 UI-DESC-TIT-CHEQ                    PIC  X(001).
          05 UI-FILLER15                         PIC  X(001)  VALUE ','.
          05 UI-CBCO-TIT                         PIC  9(003).
          05 UI-FILLER16                         PIC  X(001)  VALUE ';'.
          05 UI-PRODT-TIT                        PIC  9(002).
          05 UI-FILLER17                         PIC  X(001)  VALUE ';'.
          05 UI-CNEGOC-TIT                       PIC  9(011).
          05 UI-FILLER18                         PIC  X(001)  VALUE ';'.
          05 UI-CTITLO-TIT                       PIC  9(011).
          05 UI-FILLER19                         PIC  X(001)  VALUE ';'.
          05 UI-CBCO-CHEQ                        PIC  9(003).
          05 UI-FILLER20                         PIC  X(001)  VALUE ';'.
          05 UI-AGE-CHEQ                         PIC  9(005).
          05 UI-FILLER21                         PIC  X(001)  VALUE ';'.
          05 UI-CTA-CHEQ                         PIC  9(013).
          05 UI-FILLER22                         PIC  X(001)  VALUE ';'.
          05 UI-CHE-CHEQ                         PIC  9(007).
          05 UI-FILLER23                         PIC  X(001)  VALUE ';'.
          05 UI-CUSUAR-AUTOR                     PIC  X(009).
          05 UI-FILLER24                         PIC  X(001)  VALUE ';'.
          05 UI-FILLER-ISD                       PIC  X(070).
