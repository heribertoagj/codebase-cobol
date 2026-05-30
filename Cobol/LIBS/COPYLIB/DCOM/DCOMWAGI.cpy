      ******************************************************************
      * NOME BOOK : DCOMWAGI - BOOK DE INTERFACE FUNCIONAL             *
      * DESCRICAO : CONSULTA DADOS MINIMOS                             *
      * DATA      : 09/02/2023                                         *
      * AUTOR     : WIPRO                                              *
      * ANALISTA  : HAMILTON DE GOIS                                   *
      * COMPONENTE: RECM                                               *
      * TAMANHO   : 167 BYTES                                          *
      ******************************************************************
      *  DATA       AUTOR             DESCRICAO / MANUTENCAO           *
      ******************************************************************
           05  DCOMWAGI-HEADER.
               10  DCOMWAGI-COD-LAYOUT    PIC  X(08)   VALUE 'DCOMWAGI'.
               10  DCOMWAGI-TAM-LAYOUT    PIC  9(05)   VALUE  167.
           05  DCOMWAGI-REGISTRO.
               10  DCOMWAGI-BLOCO-ENTRADA.
                   15  DCOMWAGI-E-DANO-OPER-DESC     PIC  9(04).
                   15  DCOMWAGI-E-NSEQ-OPER-DESC     PIC  9(09).
               10  DCOMWAGI-BLOCO-SAIDA.
                   15  DCOMWAGI-S-DINIC-OPER-DESC    PIC  X(10).
                   15  DCOMWAGI-S-DVCTO-FNAL-OPER    PIC  X(10).
                   15  DCOMWAGI-S-QPCELA-OPER        PIC  9(05).
                   15  DCOMWAGI-S-VJURO-OPER-DESC    PIC  9(15)V99.
                   15  DCOMWAGI-S-VTAC-OPER-DESC     PIC  9(15)V99.
                   15  DCOMWAGI-S-VIOF-OPER-DESC     PIC  9(15)V99.
                   15  DCOMWAGI-S-PTX-JURO-MES       PIC  9(03)V99999.
                   15  DCOMWAGI-S-VCUSTO-DESC-MES    PIC  9(08)V9999999.
                   15  DCOMWAGI-S-VCUSTO-DESC-ANO    PIC  9(08)V9999999.
                   15  DCOMWAGI-S-SALDO-DEVEDOR      PIC  9(15)V99.
                   15  DCOMWAGI-S-IRSUMO-SPROD-DESC  PIC  X(10).

