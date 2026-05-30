      ******************************************************************
      * NOME BOOK : DCOMW04I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA DETALHE CONTROLE INTERNO                  *
      * DATA      : 05/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 433 BYTES                                          *
      ******************************************************************
       05 DCOMW04I-HEADER.
         10 DCOMW04I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW04I'.
         10 DCOMW04I-TAM-LAYOUT        PIC 9(05)   VALUE 433.
       05 DCOMW04I-REGISTRO.
         10 DCOMW04I-BLOCO-ENTRADA.
           15 DCOMW04I-E-CONTRATO                  PIC 9(13).
         10 DCOMW04I-BLOCO-SAIDA.
           15 DCOMW04I-S-NOME-CLIENTE              PIC X(40).
           15 DCOMW04I-S-NR-CONTRATO               PIC 9(13).
           15 DCOMW04I-S-BANCO                     PIC 9(05).
           15 DCOMW04I-S-NOME-BANCO                PIC X(70).
           15 DCOMW04I-S-CNPJ                      PIC 9(09).
           15 DCOMW04I-S-FILIAL                    PIC 9(04).
           15 DCOMW04I-S-CTRL                      PIC 9(02).
           15 DCOMW04I-S-AGENCIA                   PIC 9(05).
           15 DCOMW04I-S-NOME-AGENCIA              PIC X(50).
           15 DCOMW04I-S-CONTA-CORRENTE            PIC 9(13).
           15 DCOMW04I-S-DIGITO-CONTA              PIC X(02).
           15 DCOMW04I-S-PRODUTO                   PIC 9(03).
           15 DCOMW04I-S-DESC-PRODUTO              PIC X(40).
           15 DCOMW04I-S-COD-SUBPRODUTO            PIC 9(03).
           15 DCOMW04I-S-DESC-SUBPRODUTO           PIC X(40).
           15 DCOMW04I-S-VALOR-LIMITE              PIC 9(15)V99.
           15 DCOMW04I-S-VL-TARIFA                 PIC 9(13)V99.
           15 DCOMW04I-S-DESCR-TARIFA              PIC X(30).
           15 DCOMW04I-S-PERCENTUAL-TARIFA         PIC 9(07)V9(06).
           15 DCOMW04I-S-DATA-CELEBRACAO           PIC X(10).
           15 DCOMW04I-S-DATA-INICIO               PIC X(10).
           15 DCOMW04I-S-DATA-VENCIMENTO           PIC X(10).
           15 DCOMW04I-S-PRAZO                     PIC 9(03).
           15 DCOMW04I-QTITLO                      PIC 9(05).
           15 DCOMW04I-CMEIO-ENTRD                 PIC 9(03).
           15 DCOMW04I-CTPO-DESC                   PIC 9(03).
           15 DCOMW04I-TITLO-TELA                  PIC X(40).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
