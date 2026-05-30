      ******************************************************************
      * NOME BOOK : DCOMW25I - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA DETALHE CONTRATO      IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00586 BYTES                                        *
      ******************************************************************
       05 DCOMW25I-HEADER.
          10 DCOMW25I-COD-LAYOUT             PIC X(08) VALUE 'DCOMW25I'.
          10 DCOMW25I-TAM-LAYOUT             PIC 9(05) VALUE 00586.
       05 DCOMW25I-REGISTRO.
          10 DCOMW25I-ENTRADA.
             15 DCOMW25I-E-DANO-OPER         PIC 9(04).
             15 DCOMW25I-E-NSEQ-OPER         PIC 9(09).
          10 DCOMW25I-BLOCO-SAIDA.
             15 DCOMW25I-IPSSOA-DESC-COML    PIC X(40).
             15 DCOMW25I-CCNPJ-CPF           PIC 9(09).
             15 DCOMW25I-CFLIAL-CNPJ         PIC 9(04).
             15 DCOMW25I-CCTRL-CNPJ-CPF      PIC 9(02).
             15 DCOMW25I-CBCO                PIC 9(05).
             15 DCOMW25I-NOME-BANCO          PIC X(20).
             15 DCOMW25I-CAG-BCRIA           PIC 9(05).
             15 DCOMW25I-DIGITO-AGENCIA      PIC X(02).
             15 DCOMW25I-NOME-AGENCIA        PIC X(20).
             15 DCOMW25I-CCTA-BCRIA-CLI      PIC 9(13).
             15 DCOMW25I-DIGITO-CONTA        PIC X(02).
             15 DCOMW25I-OPERACAO            PIC 9(13).
             15 DCOMW25I-CPRODT              PIC 9(03).
             15 DCOMW25I-DESCR-PRODUTO       PIC X(40).
             15 DCOMW25I-CSPROD-DESC-COML    PIC 9(03).
             15 DCOMW25I-IRSUMO-CSPROD       PIC X(15).
             15 DCOMW25I-IRSUMO-SIT-DESC     PIC X(15).
             15 DCOMW25I-CCONTR-CONVE-DESC   PIC 9(09).
             15 DCOMW25I-CONTRATO-LIMITE     PIC 9(09).
             15 DCOMW25I-VOPER-DESC-COML     PIC 9(15)V99.
             15 DCOMW25I-VTAC-OPER-DESC      PIC 9(13)V99.
             15 DCOMW25I-VTARIF-REG-TITLO    PIC 9(13)V99.
             15 DCOMW25I-VJURO-OPER-DESC     PIC 9(13)V99.
             15 DCOMW25I-VIOF-OPER-DESC      PIC 9(13)V99.
             15 DCOMW25I-IRSUMO-TPO-TX-JURO  PIC X(15).
             15 DCOMW25I-PTX-JURO-MES        PIC 9(07)V9(06).
             15 DCOMW25I-DVCTO-FNAL-OPER     PIC X(10).
             15 DCOMW25I-GARANTIA            PIC X(40).
             15 DCOMW25I-COD-CONE            PIC X(18).
             15 DCOMW25I-VTRIBUTOS           PIC 9(15)V99.
             15 DCOMW25I-VSEGUROS            PIC 9(15)V99.
             15 DCOMW25I-VTARIFA-CET         PIC 9(15)V99.
             15 DCOMW25I-VPGTOSERV           PIC 9(15)V99.
             15 DCOMW25I-VREGISTROS          PIC 9(15)V99.
             15 DCOMW25I-VCET-TOTAL          PIC 9(15)V99.
             15 DCOMW25I-VCET-MES            PIC 9(08)V9(07).
             15 DCOMW25I-VCET-ANO            PIC 9(08)V9(07).
             15 DCOMW25I-QTITLO              PIC 9(05).
             15 DCOMW25I-CMEIO-ENTRD         PIC 9(03).
             15 DCOMW25I-CTPO-DESC           PIC 9(03).
             15 DCOMW25I-TITLO-TELA          PIC X(40).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
