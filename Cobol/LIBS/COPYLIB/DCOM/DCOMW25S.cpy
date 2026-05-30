      ******************************************************************
      * NOME BOOK : DCOMW25S - BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTA DETALHE CONTRATO      IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00573 BYTES                                        *
      ******************************************************************
       05 DCOMW25S-HEADER.
          10 DCOMW25S-COD-LAYOUT             PIC X(08) VALUE 'DCOMW25S'.
          10 DCOMW25S-TAM-LAYOUT             PIC 9(05) VALUE 00573.
       05 DCOMW25S-REGISTRO.
          10 DCOMW25S-BLOCO-SAIDA.
             15 DCOMW25S-IPSSOA-DESC-COML    PIC X(40).
             15 DCOMW25S-CCNPJ-CPF           PIC 9(09).
             15 DCOMW25S-CFLIAL-CNPJ         PIC 9(04).
             15 DCOMW25S-CCTRL-CNPJ-CPF      PIC 9(02).
             15 DCOMW25S-CBCO                PIC 9(05).
             15 DCOMW25S-NOME-BANCO          PIC X(20).
             15 DCOMW25S-CAG-BCRIA           PIC 9(05).
             15 DCOMW25S-DIGITO-AGENCIA      PIC X(02).
             15 DCOMW25S-NOME-AGENCIA        PIC X(20).
             15 DCOMW25S-CCTA-BCRIA-CLI      PIC 9(13).
             15 DCOMW25S-DIGITO-CONTA        PIC X(02).
             15 DCOMW25S-OPERACAO            PIC 9(13).
             15 DCOMW25S-CPRODT              PIC 9(03).
             15 DCOMW25S-DESCR-PRODUTO       PIC X(40).
             15 DCOMW25S-CSPROD-DESC-COML    PIC 9(03).
             15 DCOMW25I-IRSUMO-CSPROD       PIC X(15).
             15 DCOMW25S-IRSUMO-SIT-DESC     PIC X(15).
             15 DCOMW25S-CCONTR-CONVE-DESC   PIC 9(09).
             15 DCOMW25S-CONTRATO-LIMITE     PIC 9(09).
             15 DCOMW25S-VOPER-DESC-COML     PIC 9(15)V99.
             15 DCOMW25S-VTAC-OPER-DESC      PIC 9(13)V99.
             15 DCOMW25S-VTARIF-REG-TITLO    PIC 9(13)V99.
             15 DCOMW25S-VJURO-OPER-DESC     PIC 9(13)V99.
             15 DCOMW25S-VIOF-OPER-DESC      PIC 9(13)V99.
             15 DCOMW25S-IRSUMO-TPO-TX-JURO  PIC X(15).
             15 DCOMW25S-PTX-JURO-MES        PIC 9(07)V9(06).
             15 DCOMW25S-DVCTO-FNAL-OPER     PIC X(10).
             15 DCOMW25S-GARANTIA            PIC X(40).
             15 DCOMW25S-COD-CONE            PIC X(18).
             15 DCOMW25S-VTRIBUTOS           PIC 9(15)V99.
             15 DCOMW25S-VSEGUROS            PIC 9(15)V99.
             15 DCOMW25S-VTARIFA-CET         PIC 9(15)V99.
             15 DCOMW25S-VPGTOSERV           PIC 9(15)V99.
             15 DCOMW25S-VREGISTROS          PIC 9(15)V99.
             15 DCOMW25S-VCET-TOTAL          PIC 9(15)V99.
             15 DCOMW25S-VCET-MES            PIC 9(08)V9(07).
             15 DCOMW25S-VCET-ANO            PIC 9(08)V9(07).
             15 DCOMW25S-QTITLO              PIC 9(05).
             15 DCOMW25S-CMEIO-ENTRD         PIC 9(03).
             15 DCOMW25S-CTPO-DESC           PIC 9(03).
             15 DCOMW25S-TITLO-TELA          PIC X(40).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
