      *** AREA DE COMUNICACAO - IMS
      *** MENU PARAMETROS IMS SLIG0010
      ***===========================================================***
      *** NOME INC                                     LENGTH=0300  ***
      *** I#SLIGAA                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DO PROGRAMA SLIG0010         ***
      ***            COM OS PROGRAMAS CHAMADOS                      ***
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
      *
       01  SLIGAA-AREA.
           03 SLIGAA-LL                    PIC S9(004) COMP VALUE +300.
           03 SLIGAA-ZZ                    PIC S9(004) COMP VALUE +0.
           03 SLIGAA-TRANCODE              PIC  X(015).
           03 SLIGAA-SENHAS                PIC  X(037).
           03 SLIGAA-COMANDO               PIC  X(008).
           03 SLIGAA-PGM-CHAMADOR          PIC  X(008).
           03 SLIGAA-DADOS-ENVIADOS.
              05 SLIGAA-PERFIL-USUARIO     PIC  X(002).
              05 SLIGAA-CBCO               PIC  9(003).
              05 SLIGAA-CDEPDC             PIC  9(005).
              05 SLIGAA-CFUNC-BDSCO        PIC  9(009).
           03 SLIGAA-DADOS-RETORNADOS.
              05 SLIGAA-COD-RETORNO        PIC  9(002).
              05 SLIGAA-MENSAGEM           PIC  X(079).
           03 SLIGAA-DADOS-PESQUISA.
CNPJ7C        05 SLIGAA-CCGC-CPF-TELA      PIC  X(009).
              05 SLIGAA-PAG-TELA           PIC  9(008).
              05 SLIGAA-CTERM              PIC  X(008).
           03 SLIGAA-FILLER                PIC  X(103).
