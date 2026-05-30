      *---------------------------------------------------------------*
      *               B R Q                                           *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#RUEC87 - PARA MODULO RUEC8850                 *
      * GERACAO.....: DEZEMBRO/2017                                   *
      *---------------------------------------------------------------*
      * OBJETIVO....: INCLUSAO DOS DADOS RETORNADOS PELA COR0006R1    *
      *               NA TABELA RUECB046                              *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
      **
       01  RUEC87-AREA.
           05  RUEC87-ENVIO.
             10  RUEC87-PGM-CHAMADOR         PIC  X(008).
             10  RUEC87-NRO-CTRLIF           PIC  X(020).
             10  RUEC87-SISMSG               PIC  X(30000).
      **
      *************** AREA DE DADOS DE RETORNO ***************
      **
           05  RUEC87-RETORNO.
             10  RUEC87-COD-RET              PIC  9(005).
      **
      **=======  00 - OK
      **=======  01 - DADOS INCONSISTENTES
      **=======  03 - NAO ENCONTRADO
      **=======  99 - ERRO DB2
             10  RUEC87-MENSAGEM             PIC  X(079).
             10  FILLER                      PIC  X(050).
             10  RUEC87-SQLCA                PIC  X(136).
             10  RUEC87-ERRO-AREA            PIC  X(107).
