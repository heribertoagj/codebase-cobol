      ******************************************************************
      * NOME BOOK : I#DCOMZG                                           *
      * DESCRI#AO : BOOK DE INTERFACE PARA CONSULTAR AGENDA DO CLIENTE *
      * DATA      : 08/2020                                            *
      * AUTOR     : DANIELA GALVAO - CAPGEMINI - ALPHA                 *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * TAMANHO   : 0500 BYTES                                         *
      ******************************************************************
          03 DCOMZG-REGISTRO.
           05 DCOMZG-CCUSTO               PIC X(04).
           05 DCOMZG-DT-MOVTO             PIC X(10).
           05 DCOMZG-BANCO                PIC 9(03).
           05 DCOMZG-AGENCIA              PIC 9(05).
           05 DCOMZG-CONTA                PIC 9(13).
           05 DCOMZG-BANDEIRA             PIC 9(05).
           05 DCOMZG-VLR-PRODT            PIC 9(15)V9(002) COMP-3.
           05 DCOMZG-DT-UR                PIC X(10).
           05 DCOMZG-ROT-PARC             PIC X(01).
           05 DCOMZG-DT-INICIO            PIC X(10).
           05 DCOMZG-DT-FIM               PIC X(10).
           05 DCOMZG-QTD-TITULOS          PIC S9(05) COMP-3.
           05 DCOMZG-VL-DESCONTO          PIC S9(13)V99 COMP-3.
           05 DCOMZG-TAXA-JUROS           PIC S9(03)V99 COMP-3.
           05 DCOMZG-COD-AUTORIZ          PIC S9(07)    COMP-3.
           05 DCOMZG-NOME-AUTORIZ         PIC X(40).
           05 DCOMZG-OPERACAO.
              07 DCOMZG-DANO-OPER-DESC    PIC S9(04)    COMP-3.
              07 DCOMZG-NSEQ-OPER-DESC    PIC S9(09)    COMP-3.
           05 DCOMZG-IOF-DESCONTO         PIC S9(15)V99 COMP-3.
           05 DCOMZG-TAC-DESCONTO         PIC S9(15)V99 COMP-3.
           05 DCOMZG-VLR-JUROS-DESC       PIC S9(15)V99 COMP-3.
           05 DCOMZG-VLIQ-DESCONTO        PIC S9(15)V99 COMP-3.
           05 DCOMZG-COD-OCORRENCIA       PIC X(01).
           05 DCOMZG-COD-RETORNO          PIC 9(04).
           05 DCOMZG-MENSAGEM-RETORNO     PIC X(80).
           05 DCOMZG-ORIGEM-CADASTRO      PIC 9(001).
           05 DCOMZG-CNPJ-6408            PIC X(015).
           05 DCOMZG-TP-PESSOA-6408       PIC X(001).
           05 DCOMZG-CSGMTO-CLI-6432      PIC 9(003).
           05 DCOMZG-CPRODT               PIC 9(003).
           05 DCOMZG-CSPROD-DESC-COML     PIC 9(003).
           05 DCOMZG-COOBRIGACAO          PIC 9(001).
           05 DCOMZG-CDEPDC-G4RT          PIC 9(005).
           05 DCOMZG-CSEQ-STUDO-G4RT      PIC 9(009).
           05 DCOMZG-CCART-64RT           PIC X(003).
           05 DCOMZG-AJUSTE-CONE          PIC 9(15)V9(002) COMP-3.
           05 DCOMZG-AJUSTE-OPER-VLR      PIC S9(13)V9(002) COMP-3.
           05 DCOMZG-CANC-OPER            PIC X(001).
           05 DCOMZG-FILLER               PIC X(171).
