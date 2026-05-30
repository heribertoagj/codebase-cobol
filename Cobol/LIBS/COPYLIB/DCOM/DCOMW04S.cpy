      ******************************************************************
      * NOME BOOK : DCOMW04S - BOOK DE SAIDA     COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA DETALHE CONTROLE INTERNO                  *
      * DATA      : 06/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 420 BYTES                                          *
      ******************************************************************
       05 DCOMW04S-HEADER.
         10 DCOMW04S-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW04S'.
         10 DCOMW04S-TAM-LAYOUT        PIC 9(05)   VALUE 420.
       05 DCOMW04S-REGISTRO.
         10 DCOMW04S-BLOCO-SAIDA.
           15 DCOMW04S-S-NOME-CLIENTE              PIC X(40).
           15 DCOMW04S-S-NR-CONTRATO               PIC 9(13).
           15 DCOMW04S-S-BANCO                     PIC 9(05).
           15 DCOMW04S-S-NOME-BANCO                PIC X(70).
           15 DCOMW04S-S-CNPJ                      PIC 9(09).
           15 DCOMW04S-S-FILIAL                    PIC 9(04).
           15 DCOMW04S-S-CTRL                      PIC 9(02).
           15 DCOMW04S-S-AGENCIA                   PIC 9(05).
           15 DCOMW04S-S-NOME-AGENCIA              PIC X(50).
           15 DCOMW04S-S-CONTA-CORRENTE            PIC 9(13).
           15 DCOMW04S-S-DIGITO-CONTA              PIC X(02).
           15 DCOMW04S-S-PRODUTO                   PIC 9(03).
           15 DCOMW04S-S-DESC-PRODUTO              PIC X(40).
           15 DCOMW04S-S-COD-SUBPRODUTO            PIC 9(03).
           15 DCOMW04S-S-DESC-SUBPRODUTO           PIC X(40).
           15 DCOMW04S-S-VALOR-LIMITE              PIC 9(15)V99.
           15 DCOMW04S-S-VL-TARIFA                 PIC 9(13)V99.
           15 DCOMW04S-S-DESCR-TARIFA              PIC X(30).
           15 DCOMW04S-S-PERCENTUAL-TARIFA         PIC 9(07)V9(06).
           15 DCOMW04S-S-DATA-CELEBRACAO           PIC X(10).
           15 DCOMW04S-S-DATA-INICIO               PIC X(10).
           15 DCOMW04S-S-DATA-VENCIMENTO           PIC X(10).
           15 DCOMW04S-S-PRAZO                     PIC 9(03).
           15 DCOMW04S-QTITLO                      PIC 9(05).
           15 DCOMW04S-CMEIO-ENTRD                 PIC 9(03).
           15 DCOMW04S-CTPO-DESC                   PIC 9(03).
           15 DCOMW04S-TITLO-TELA                  PIC X(40).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
