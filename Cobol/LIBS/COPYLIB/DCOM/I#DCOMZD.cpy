      ******************************************************************
      * NOME BOOK : I#DCOMZD                                           *
      * DESCRICAO : BOOK DE INTERFACE PARA CONSULTAR INFORMACOES DE    *
      *           : PROTOCOLO                                          *
      * DATA      : 09/2020                                            *
      * AUTOR     : FELIPE MENDES - CAPGEMINI - ALPHA                  *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      * TAMANHO   : 0217 BYTES                                         *
      ******************************************************************
      *
       01  DCOMZD-REGISTRO.
           05 DCOMZD-BLOCO-ENTRADA.
              10 DCOMZD-E-CTPO-PROT-AGNDA         PIC  X(001).
              10 DCOMZD-E-NSMULA-OPER-DESC        PIC S9(009).
              10 DCOMZD-E-DANO-OPER-DESC          PIC S9(004).
              10 DCOMZD-E-NSEQ-OPER-DESC          PIC S9(009).
           05 DCOMZD-RETORNO.
              10 DCOMZD-S-COD-RETORNO             PIC  9(004).
              10 DCOMZD-S-MSG-RETORNO             PIC  X(083).
              10 DCOMZD-S-CBCO                    PIC S9(003).
              10 DCOMZD-S-CAG-BCRIA               PIC S9(005).
              10 DCOMZD-S-CCTA-BCRIA-CLI          PIC S9(013).
              10 DCOMZD-S-CPROT-SOLTC-AGNDA       PIC  X(050).
              10 DCOMZD-S-QTEMPO-VALDD-PROCM      PIC S9(003).
              10 DCOMZD-S-CSIT-PROT-AGNDA         PIC S9(001).
              10 DCOMZD-S-HINCL-REG               PIC  X(026).
              10 DCOMZD-S-CTPO-CANAL              PIC S9(003).
              10 DCOMZD-S-QTEMPO-PROCM-CANAL      PIC S9(003).
