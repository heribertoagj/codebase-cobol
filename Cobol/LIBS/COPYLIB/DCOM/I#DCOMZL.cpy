      ******************************************************************
      * NOME BOOK : I#DCOMZL                                           *
      * DESCRICAO : BOOK DE INTERFACE PARA CANCELAR OPERACAO NA CIP    *
      * DATA      : 09/2020                                            *
      * AUTOR     : ULISSES ASSIS - CAPGEMINI - ALPHA                  *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      * TAMANHO   : 0100 BYTES                                         *
      ******************************************************************
       01  DCOMZL-REGISTRO.
              10 DCOMZL-BLOCO-ENTRADA.
                 15 DCOMZL-E-CBCO                 PIC  9(003).
                 15 DCOMZL-E-CAG-BCRIA            PIC  9(005).
                 15 DCOMZL-E-CCTA-BCRIA-CLI       PIC  9(013).
                 15 DCOMZL-E-DANO-OPER-DESC       PIC  9(004).
                 15 DCOMZL-E-NSEQ-OPER-DESC       PIC  9(009).
                 15 DCOMZL-E-CTPO-PROT-AGNDA      PIC  X(001).
                 15 DCOMZL-E-CANAL                PIC  9(003).
           05 DCOMZL-SAIDA.
                15 DCOMZL-S-CPROT-SOLTC-AGNDA     PIC  X(050).
                15 DCOMZL-S-CSIT-PROT-AGNDA       PIC  9(001).
                15 DCOMZL-S-QTEMPO-PROTOCOLO      PIC  9(003).
                15 DCOMZL-S-QTEMPO-CANAL          PIC  9(003).
                15 DCOMZL-S-COD-RETORNO           PIC  9(002).
                15 DCOMZL-S-MSG-RETORNO           PIC  X(050).
