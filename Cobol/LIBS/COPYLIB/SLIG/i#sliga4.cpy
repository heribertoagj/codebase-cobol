      *** AREA DE COMUNICACAO - IMS
      ***
      ***===========================================================***
      *** NOME INC                                     LENGTH=0300  ***
      *** I#SLIGA4                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DOS PROGRAMAS                ***
      ***            UI - DEPENDENCIA                               ***
      ***===========================================================***
      *                                                                        %
       01  SLIGA4-AREA.
           03 SLIGA4-LL                    PIC S9(004) COMP VALUE +300.
           03 SLIGA4-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGA4-TRANCODE              PIC  X(015).
           03 SLIGA4-SENHAS                PIC  X(037).
           03 SLIGA4-COMANDO               PIC  X(008).
           03 SLIGA4-DADOS-ENVIADOS.
              05 SLIGA4-PERFIL-USUARIO     PIC  X(002).
              05 SLIGA4-CBCO               PIC  9(003).
              05 SLIGA4-CDEPDC             PIC  9(005).
              05 SLIGA4-CFUNC-BDSCO        PIC  9(009).
           03 SLIGA4-DADOS-RETORNADOS.
              05 SLIGA4-COD-RETORNO        PIC  9(002).
              05 SLIGA4-MENSAGEM           PIC  X(079).
           03 SLIGA4-DADOS-PESQUISA.
              05 SLIGA4-CBCOP              PIC  9(003).
              05 SLIGA4-CDEPDCP            PIC  9(005).
              05 SLIGA4-CCGC-CPF           PIC  X(009).
              05 SLIGA4-CINDCD-PSSOA       PIC  X(001).
              05 SLIGA4-CBCO-SACDO         PIC  9(003).
              05 SLIGA4-CAG-BCRIA-SACDO    PIC  9(005).
              05 SLIGA4-CCTA-BCRIA-SACDO   PIC  9(013).
              05 SLIGA4-CCGC-CPF-TELA      PIC  X(009).
              05 SLIGA4-PAG-TELA           PIC  9(008).
      * FORMATO DATE - DD.MM.AAAA
              05 SLIGA4-PERIODO-INI        PIC  X(010).
      * FORMATO DATE - DD.MM.AAAA
              05 SLIGA4-PERIODO-FIM        PIC  X(010).
              05 SLIGA4-PAG-TELA-2         PIC  9(003).
              05 SLIGA4-SEQ-BANDA          PIC  9(015).
           03 SLIGA4-NOME-EMITENTE         PIC  X(040).
           03 FILLER                       PIC  X(002).
