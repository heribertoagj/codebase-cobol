      ******************************************************************
      * NOME BOOK : DCOMW30I - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA DETALHE CONTRATO      IMPRESSAO SAIDA     *
      * DATA      : 27/03/2023                                         *
      * AUTOR     : RODRIGO PEREIRA DOS SANTOS                         *
      * EMPRESA   : FOURSYS                                            *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00586 BYTES                                        *
      ******************************************************************
       05 DCOMW30I-HEADER.
          10 DCOMW30I-COD-LAYOUT             PIC X(08) VALUE 'DCOMW30I'.
          10 DCOMW30I-TAM-LAYOUT             PIC 9(05) VALUE 00586.
       05 DCOMW30I-REGISTRO.
          10 DCOMW30I-ENTRADA.
             15 DCOMW30I-E-DANO-OPER         PIC 9(04).
             15 DCOMW30I-E-NSEQ-OPER         PIC 9(09).
      *******DADOS OBTIDOS NA SESSAO*******************************
             15 DCOMW30I-E-CUSUAR-SESSAO      PIC X(09).
             15 DCOMW30I-E-CDEPEN-SESSAO      PIC 9(05).
          10 DCOMW30I-BLOCO-SAIDA.
             15 DCOMW30I-IPSSOA-DESC-COML    PIC X(40).
             15 DCOMW30I-CCNPJ-CPF           PIC 9(09).
             15 DCOMW30I-CFLIAL-CNPJ         PIC 9(04).
             15 DCOMW30I-CCTRL-CNPJ-CPF      PIC 9(02).
             15 DCOMW30I-CBCO                PIC 9(05).
             15 DCOMW30I-NOME-BANCO          PIC X(20).
             15 DCOMW30I-CAG-BCRIA           PIC 9(05).
             15 DCOMW30I-DIGITO-AGENCIA      PIC X(02).
             15 DCOMW30I-NOME-AGENCIA        PIC X(20).
             15 DCOMW30I-CCTA-BCRIA-CLI      PIC 9(13).
             15 DCOMW30I-DIGITO-CONTA        PIC X(02).
             15 DCOMW30I-OPERACAO            PIC 9(13).
             15 DCOMW30I-CPRODT              PIC 9(03).
             15 DCOMW30I-DESCR-PRODUTO       PIC X(40).
             15 DCOMW30I-CSPROD-DESC-COML    PIC 9(03).
             15 DCOMW30I-IRSUMO-CSPROD       PIC X(15).
             15 DCOMW30I-IRSUMO-SIT-DESC     PIC X(15).
             15 DCOMW30I-CCONTR-CONVE-DESC   PIC 9(09).
             15 DCOMW30I-CONTRATO-LIMITE     PIC 9(09).
             15 DCOMW30I-VOPER-DESC-COML     PIC 9(15)V99.
             15 DCOMW30I-VTAC-OPER-DESC      PIC 9(13)V99.
             15 DCOMW30I-VTARIF-REG-TITLO    PIC 9(13)V99.
             15 DCOMW30I-VJURO-OPER-DESC     PIC 9(13)V99.
             15 DCOMW30I-VIOF-OPER-DESC      PIC 9(13)V99.
             15 DCOMW30I-IRSUMO-TPO-TX-JURO  PIC X(15).
             15 DCOMW30I-PTX-JURO-MES        PIC 9(07)V9(06).
             15 DCOMW30I-DVCTO-FNAL-OPER     PIC X(10).
             15 DCOMW30I-GARANTIA            PIC X(40).
             15 DCOMW30I-COD-CONE            PIC X(18).
             15 DCOMW30I-VTRIBUTOS           PIC 9(15)V99.
             15 DCOMW30I-VSEGUROS            PIC 9(15)V99.
             15 DCOMW30I-VTARIFA-CET         PIC 9(15)V99.
             15 DCOMW30I-VPGTOSERV           PIC 9(15)V99.
             15 DCOMW30I-VREGISTROS          PIC 9(15)V99.
             15 DCOMW30I-VCET-TOTAL          PIC 9(15)V99.
             15 DCOMW30I-VCET-MES            PIC 9(08)V9(07).
             15 DCOMW30I-VCET-ANO            PIC 9(08)V9(07).
             15 DCOMW30I-QTITLO              PIC 9(05).
             15 DCOMW30I-CMEIO-ENTRD         PIC 9(03).
             15 DCOMW30I-CTPO-DESC           PIC 9(03).
             15 DCOMW30I-TITLO-TELA          PIC X(40).
             15 DCOMW30I-S-NVIAS-ADITAMENTO          PIC 9(02).
             15 DCOMW30I-S-NOCOR                     PIC 9(02).
             15 DCOMW30I-S-QTD-TOT-PCELAS    OCCURS 0 TO 30 TIMES
                DEPENDING ON DCOMW30I-S-NOCOR.
                20 DCOMW30I-S-NUM-PARCELA            PIC 9(05).
                20 DCOMW30I-S-NOME-SACADO            PIC X(40).
                20 DCOMW30I-S-DATA-VENC-ORIG         PIC 9(10).
                20 DCOMW30I-S-DATA-VENC-PRRG         PIC 9(10).
                20 DCOMW30I-S-VALOR-ENCARGO          PIC 9(15)V99.
                20 DCOMW30I-S-VLR-TIT-AT             PIC 9(15)V99.
                20 DCOMW30I-S-TAX-JUROS-AM-AT        PIC 9(6)V9(7).
                20 DCOMW30I-S-TAX-JUROS-AA-AT        PIC 9(6)V9(7).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
