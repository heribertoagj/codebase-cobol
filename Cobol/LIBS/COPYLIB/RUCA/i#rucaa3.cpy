      *---------------------------------------------------------------*
      *                    P R I M E   I T                            *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#RUCAA3 - PARA MODULO RUCA8516                 *
      * GERACAO.....: JANEIRO/2023                                    *
      *---------------------------------------------------------------*
      * OBJETIVO....: IDENTIFICA SE CONTRATO ESTA LIQUIDADO OU NAO    *
      *---------------------------------------------------------------*
      **--------------------------------------------------------------*
       01 RUCAA3-REGISTRO.
          03 RUCAA3-ENTRADA.
             05 RUCAA3-E-CONTR            PIC 9(009).
             05 RUCAA3-E-CONTR-R REDEFINES RUCAA3-E-CONTR
                                          PIC X(009).
          03 RUCAA3-RETORNO.
             05 RUCAA3-COD-RETORNO        PIC 9(02).
             05 RUCAA3-MSG-RETORNO        PIC X(79).
      * *** SIM - CONTRATO LIQUIDADO        (SEM PARCELAS PENDENTES)
      * *** NAO - CONTRATO NAO LIQUIDADO    (COM PARCELAS PENDNETES)
          03 RUCAA3-SAIDA.
             05 RUCAA3-S-CONTR-LIQUIDADO  PIC X(03).
