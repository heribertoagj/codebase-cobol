      ******************************************************************
      * NOME BOOK : I#DCOMZE                                           *
      * DESCRI#AO : BOOK DE INTERFACE PARA CONSULTAR AGENDA DO CLIENTE *
      * DATA      : 08/2020                                            *
      * AUTOR     : ULISSES ASSIS - CAPGEMINI - ALPHA                  *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL
      * TAMANHO   : 1705 BYTES                                         *
      ******************************************************************
       01  DCOMZE-BLOCO-ENTRADA.
           05 DCOMZE-E-CANAL                     PIC X(08).
           05 DCOMZE-E-AGENCIA                   PIC 9(05).
           05 DCOMZE-E-CONTA                     PIC 9(13).
           05 DCOMZE-E-PROD-REST                 PIC 9(05).
           05 DCOMZE-E-PROTOCOLO                 PIC X(50).
       01  DCOMZE-SAIDA.
           05 DCOMZE-S-AGENCIA                   PIC 9(05).
           05 DCOMZE-S-CONTA                     PIC 9(13).
           05 DCOMZE-S-PROD-REST                 PIC 9(05).
           05 DCOMZE-S-QTD-OCOR                  PIC 9(02).
           05 DCOMZE-S-INDC-REST                 PIC X(01).
           05 DCOMZE-S-PROTOCOLO                 PIC X(50).
           05 DCOMZE-S-CSIT-PROT-AGNDA           PIC 9(01).
           05 DCOMZE-S-QTEMPO-TELEBNCO           PIC X(08).
           05 DCOMZE-S-OCORRENCIAS   OCCURS 16 TIMES.
              10 DCOMZE-S-COD-PRODUTO            PIC 9(05).
              10 DCOMZE-S-PRODUTO                PIC X(25).
              10 DCOMZE-S-CREDITO-SIN            PIC X(01).
              10 DCOMZE-S-CREDITO                PIC 9(13)V9(2).
              10 DCOMZE-S-ANTEC-SIN              PIC X(01).
              10 DCOMZE-S-ANTEC                  PIC 9(13)V9(2).
              10 DCOMZE-S-SALDO-TOTAL-SIN        PIC X(01).
              10 DCOMZE-S-SALDO-TOTAL            PIC 9(13)V9(2).
           05 DCOMZE-S-TOTGER-CREDITO-SIN        PIC X(01).
           05 DCOMZE-S-TOTGER-CREDITO            PIC 9(13)V99.
           05 DCOMZE-S-TOTGER-ANTEC-SIN          PIC X(01).
           05 DCOMZE-S-TOTGER-ANTEC              PIC 9(13)V99.
           05 DCOMZE-S-TOTGER-SALDO-SIN          PIC X(01).
           05 DCOMZE-S-TOTGER-SALDO              PIC 9(13)V99.
           05 DCOMZE-S-DISP-ANTEC                PIC 9(13)V99.
           05 DCOMZE-S-TOTAL-ANTEC               PIC 9(13)V99.
           05 DCOMZE-S-SALDO-DISP-ANTEC-SIN      PIC X(01).
           05 DCOMZE-S-SALDO-DISP-ANTEC          PIC 9(13)V99.
           05 DCOMZE-S-NOME-FANTASIA             PIC X(35).
           05 DCOMZE-S-NOME-RAZAO                PIC X(40).
           05 DCOMZE-S-ENDERECO                  PIC X(40).
           05 DCOMZE-S-CEP-5                     PIC X(05).
           05 DCOMZE-S-CEP-3                     PIC X(03).
           05 DCOMZE-S-CCGC-CPF                  PIC 9(09).
           05 DCOMZE-S-CFILIAL-CGC               PIC 9(04).
           05 DCOMZE-S-CCTRL-CPF-CGC             PIC 9(02).
           05 DCOMZE-S-TIPO-PESSOA               PIC X(02).
           05 DCOMZE-S-BLOQUEIO                  PIC X(01).
           05 DCOMZE-S-LOCAL                     PIC X(02).
           05 DCOMZE-S-COD-RETORNO               PIC X(02).
           05 DCOMZE-S-MSG-RETORNO               PIC X(79).
