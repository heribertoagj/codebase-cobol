      ******************************************************************
      * NOME BOOK : I#DCOMZC                                           *
      * DESCRICAO : BOOK DE INTERFACE PARA CONSULTAR AGENDA DO CLIENTE *
      * DATA      : 08/2020                                            *
      * AUTOR     : ULISSES ASSIS - CAPGEMINI - ALPHA                  *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      * TAMANHO   : 3383 BYTES                                         *
      ******************************************************************
      *
       01  DCOMZC-REGISTRO.
           05 DCOMZC-AREA-ENTRADA.
              10 DCOMZC-E-CTERM                 PIC  X(008).
              10 DCOMZC-E-CBCO                  PIC  9(003).
              10 DCOMZC-E-CAG-BCRIA             PIC  9(005).
              10 DCOMZC-E-CCTA-BCRIA-CLI        PIC  9(007).
              10 DCOMZC-E-PRODUTO               PIC  9(005).
              10 DCOMZC-E-DT-INI                PIC  X(010).
              10 DCOMZC-E-DT-FIM                PIC  X(010).
              10 DCOMZC-E-VLR-OPERACAO          PIC  9(015)V9(2).
              10 DCOMZC-E-VLR-MIN-UR            PIC  9(015)V9(2).
              10 DCOMZC-E-VLR-MAX-UR            PIC  9(015)V9(2).
              10 DCOMZC-E-CPROT-SOLTC-AGNDA     PIC  X(050).
              10 DCOMZC-E-CPROT-SELECT          PIC  X(001).
           05 DCOMZC-AREA-RESTART.
              10 DCOMZC-R-PROD-REST             PIC  9(005).
              10 DCOMZC-R-DT-REST               PIC  X(010).
           05 DCOMZC-AREA-SAIDA.
              10 DCOMZC-S-COD-RETORNO           PIC  9(004).
              10 DCOMZC-S-MSG-RETORNO           PIC  X(079).
              10 DCOMZC-S-CSIT-PROT-AGNDA       PIC  9(001).
              10 DCOMZC-S-QTEMPO-TELEBNCO       PIC  9(003).
              10 DCOMZC-S-QTEMPO-NETEMPRS       PIC  9(003).
              10 DCOMZC-S-QTEMPO-TERMGERE       PIC  9(003).
              10 DCOMZC-S-NUM-CONSULTAS         PIC  9(005).
              10 DCOMZC-S-RES OCCURS 60 TIMES.
                 15 DCOMZC-S-PRODUTO            PIC  9(005).
                 15 DCOMZC-S-DESC-PROD          PIC  X(020).
                 15 DCOMZC-S-DATA               PIC  X(010).
                 15 DCOMZC-S-VLR-DISP           PIC  9(015)V99.
