      ******************************************************************
      * NOME BOOK : I#DCOMZH                                           *
      * DESCRICAO : BOOK DE INTERFACE PARA INSERIR PROTOCOLO E CANAL   *
      * DATA      : 09/2020                                            *
      * AUTOR     : AMANDA BELTOSO  - CAPGEMINI - SALVADOR             *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      * TAMANHO   : 0223 BYTES                                         *
      ******************************************************************
      *                                                                *
      * DCOMZH-E-ACAO...: 'U' UPDATE OU 'I' INSERT'                    *
      *                                                                *
      ******************************************************************
      *
       01  DCOMZH-REGISTRO.
           05 DCOMZH-BLOCO-ENTRADA.
      *       DADOS DO PROTOCOLO
              10 DCOMZH-E-CBCO                    PIC S9(003).
              10 DCOMZH-E-CAG-BCRIA               PIC S9(005).
              10 DCOMZH-E-CCTA-BCRIA-CLI          PIC S9(013).
              10 DCOMZH-E-CPROT-SOLTC-AGNDA       PIC  X(050).
              10 DCOMZH-E-CTPO-PROT-AGNDA         PIC  X(001).
              10 DCOMZH-E-QTEMPO-VALDD-PROCM      PIC S9(003).
              10 DCOMZH-E-CSIT-PROT-AGNDA         PIC S9(001).
              10 DCOMZH-E-HINCL-REG               PIC  X(026).
              10 DCOMZH-E-NSMULA-OPER-DESC        PIC S9(009).
              10 DCOMZH-E-DANO-OPER-DESC          PIC S9(004).
              10 DCOMZH-E-NSEQ-OPER-DESC          PIC S9(009).
      *       DADOS DO CANAL
              10 DCOMZH-E-CCANAL-DESC             PIC  X(008).
              10 DCOMZH-E-QTEMPO-PROCM-CANAL      PIC S9(003).
              10 DCOMZH-E-ACAO                    PIC  X(001).
           05 DCOMZH-RETORNO.
              10 DCOMZH-S-COD-RETORNO             PIC  9(004).
              10 DCOMZH-S-MSG-RETORNO             PIC  X(083).
