      *****************************************************************
      *   I#BVVE21 - AREA DE ENTRADA E SAIDA DE DADOS                 *
      *          (COMUNICACAO ENTRE TELAS E ROTEADOR  )               *
      *                                                               *
      *                   CODIGOS DE RETORNO:                         *
      *                       00 - ACESSO OK                          *
      *                       01 - ERRO DE CONSISTENCIA               *
      *                       02 - ERRO LOGICO                        *
      *                       03 - NENHUM REGISTRO ENCONTRADO         *
      *                       99 - ERRO DE ACESSO DB2                 *
      *                                                               *
      *****************************************************************

       01  BVVE21-AREA-ENTRADA.
           03 BVVE21-DADOS-ENTRADA.
              05 BVVE21-COMANDO              PIC X(0004).
              05 BVVE21-MODULO               PIC X(0008).
              05 BVVE21-PARAMETROS           PIC X(2048).
              05 BVVE21-TAM-REGISTRO         PIC 9(0004).
           03 BVVE21-DADOS-SAIDA.
              05 BVVE21-COD-RETORNO          PIC 9(0002).
              05 BVVE21-MENSAGEM             PIC X(0255).
