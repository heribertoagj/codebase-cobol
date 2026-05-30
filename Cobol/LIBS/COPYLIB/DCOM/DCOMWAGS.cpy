      ******************************************************************
      * NOME BOOK : DCOMWAGS - LAYOUT DE SAIDA                         *
      * DESCRICAO : CONSULTA DADOS MINIMOS                             *
      * DATA      : 09/02/2023                                         *
      * EMPRESA   : WIPRO                                              *
      * ANALISTA  : HAMILTON DE GOIS                                   *
      * COMPONENTE: RECM                                               *
      * TAMANHO   : 154 BYTES                                          *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
       05  DCOMWAGS-HEADER.
           10  DCOMWAGS-COD-LAYOUT        PIC  X(08)  VALUE 'DCOMWAGS'.
           10  DCOMWAGS-TAM-LAYOUT        PIC  9(05)  VALUE 154.
       05  DCOMWAGS-REGISTRO.
           10  DCOMWAGS-BLOCO-SAIDA.
               15  DCOMWAGS-DINIC-OPER-DESC    PIC  X(10).
               15  DCOMWAGS-DVCTO-FNAL-OPER    PIC  X(10).
               15  DCOMWAGS-QPCELA-OPER        PIC  9(05).
               15  DCOMWAGS-VJURO-OPER-DESC    PIC  9(15)V99.
               15  DCOMWAGS-VTAC-OPER-DESC     PIC  9(15)V99.
               15  DCOMWAGS-VIOF-OPER-DESC     PIC  9(15)V99.
               15  DCOMWAGS-PTX-JURO-MES       PIC  9(03)V99999.
               15  DCOMWAGS-VCUSTO-DESC-MES    PIC  9(08)V9999999.
               15  DCOMWAGS-VCUSTO-DESC-ANO    PIC  9(08)V9999999.
               15  DCOMWAGS-SALDO-DEVEDOR      PIC  9(15)V99.
               15  DCOMWAGS-IRSUMO-SPROD-DESC  PIC  X(10).

