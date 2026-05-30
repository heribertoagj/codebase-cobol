      ******************************************************************
      * NOME BOOK : DCOMW30S - BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTA DETALHE CONTRATO      IMPRESSAO SAIDA     *
      * DATA      : 27/03/2023                                         *
      * AUTOR     : RODRIGO PEREIRA DOS SANTOS                         *
      * EMPRESA   : FOURSYS                                            *
      * COMPONENTE:                                                    *
      * TAMANHO   : 25680 BYTES                                        *
      ******************************************************************
       05 DCOMW30S-HEADER.
          10 DCOMW30S-COD-LAYOUT             PIC X(08) VALUE 'DCOMW30S'.
          10 DCOMW30S-TAM-LAYOUT             PIC 9(05) VALUE 25680.
       05 DCOMW30S-REGISTRO.
          10 DCOMW30S-BLOCO-SAIDA.
             15 DCOMW30S-IPSSOA-DESC-COML    PIC X(40).
             15 DCOMW30S-CCNPJ-CPF           PIC 9(09).
             15 DCOMW30S-CFLIAL-CNPJ         PIC 9(04).
             15 DCOMW30S-CCTRL-CNPJ-CPF      PIC 9(02).
             15 DCOMW30S-CBCO                PIC 9(05).
             15 DCOMW30S-NOME-BANCO          PIC X(20).
             15 DCOMW30S-CAG-BCRIA           PIC 9(05).
             15 DCOMW30S-DIGITO-AGENCIA      PIC X(02).
             15 DCOMW30S-NOME-AGENCIA        PIC X(20).
             15 DCOMW30S-CCTA-BCRIA-CLI      PIC 9(13).
             15 DCOMW30S-DIGITO-CONTA        PIC X(02).
             15 DCOMW30S-OPERACAO            PIC 9(13).
             15 DCOMW30S-CPRODT              PIC 9(03).
             15 DCOMW30S-DESCR-PRODUTO       PIC X(40).
             15 DCOMW30S-CSPROD-DESC-COML    PIC 9(03).
             15 DCOMW25I-IRSUMO-CSPROD       PIC X(15).
             15 DCOMW30S-IRSUMO-SIT-DESC     PIC X(15).
             15 DCOMW30S-CCONTR-CONVE-DESC   PIC 9(09).
             15 DCOMW30S-CONTRATO-LIMITE     PIC 9(09).
             15 DCOMW30S-VOPER-DESC-COML     PIC 9(15)V99.
             15 DCOMW30S-VTAC-OPER-DESC      PIC 9(13)V99.
             15 DCOMW30S-VTARIF-REG-TITLO    PIC 9(13)V99.
             15 DCOMW30S-VJURO-OPER-DESC     PIC 9(13)V99.
             15 DCOMW30S-VIOF-OPER-DESC      PIC 9(13)V99.
             15 DCOMW30S-IRSUMO-TPO-TX-JURO  PIC X(15).
             15 DCOMW30S-PTX-JURO-MES        PIC 9(07)V9(06).
             15 DCOMW30S-DVCTO-FNAL-OPER     PIC X(10).
             15 DCOMW30S-GARANTIA            PIC X(40).
             15 DCOMW30S-COD-CONE            PIC X(18).
             15 DCOMW30S-VTRIBUTOS           PIC 9(15)V99.
             15 DCOMW30S-VSEGUROS            PIC 9(15)V99.
             15 DCOMW30S-VTARIFA-CET         PIC 9(15)V99.
             15 DCOMW30S-VPGTOSERV           PIC 9(15)V99.
             15 DCOMW30S-VREGISTROS          PIC 9(15)V99.
             15 DCOMW30S-VCET-TOTAL          PIC 9(15)V99.
             15 DCOMW30S-VCET-MES            PIC 9(08)V9(07).
             15 DCOMW30S-VCET-ANO            PIC 9(08)V9(07).
             15 DCOMW30S-QTITLO              PIC 9(05).
             15 DCOMW30S-CMEIO-ENTRD         PIC 9(03).
             15 DCOMW30S-CTPO-DESC           PIC 9(03).
             15 DCOMW30S-TITLO-TELA          PIC X(40).
             15 DCOMW30S-S-NVIAS-ADITAMENTO          PIC 9(02).
             15 DCOMW30S-S-NOCOR                     PIC 9(02).
             15 DCOMW30S-S-QTD-TOT-PCELAS    OCCURS 0 TO 30 TIMES
                DEPENDING ON DCOMW30S-S-NOCOR.
                20 DCOMW30S-S-NUM-PARCELA            PIC 9(05).
                20 DCOMW30S-S-NOME-SACADO            PIC X(40).
                20 DCOMW30S-S-DATA-VENC-ORIG         PIC X(10).
                20 DCOMW30S-S-DATA-VENC-PRRG         PIC X(10).
                20 DCOMW30S-S-VALOR-ENCARGO          PIC X(25).
                20 DCOMW30S-S-VLR-TIT-AT             PIC X(25).
                20 DCOMW30S-S-TAX-JUROS-AM-AT        PIC X(20).
                20 DCOMW30S-S-TAX-JUROS-AA-AT        PIC X(20).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
