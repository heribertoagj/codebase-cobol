      *----------------------------------------------------------------*
      *    INC = I#DCOMXI                                              *
      *----------------------------------------------------------------*
      *        AREA DE COMUNICACAO DO MODULO DCOM6443                  *
      *             OBTER PRAZO MININO PARA SELECAO DE CHEQUES         *
      *                                                                *
      ******************************************************************
       01  6443-AREA-REG.
           05 6443-ENTRADA.
              10 6443-DATA-PROC            PIC 9(009) COMP-3.
           05 6443-RETORNO.
              10 6443-COD-RETORNO          PIC 9(002).
      *          00  EXECUCAO NORMAL
      *          02  DATA INFORMADA INVALIDA
      *          04  REGISTRO INEXISTENTE
      *          05  SISTEMA INDISPONIVEL - TABELAS EM ATUALIZACAO
      *          88  ERRO NO ACESSO A TABELA DB2
              10 6443-DATA-INICIO-LNOVO    PIC 9(009) COMP-3.
              10 6443-DATA-INICIO-LCUST    PIC 9(009) COMP-3.
              10 6443-COD-ERR              PIC S9(04).
              10 6443-MENSAGEM             PIC X(050).
              10 6443-ERRO-AREA            PIC X(107).
              10 6443-SQLCA                PIC X(136).
