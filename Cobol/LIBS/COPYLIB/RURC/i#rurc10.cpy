      *****************************************************************
      *    SISTEMA : RURC - I#RURC10                                  *
      *    AREA DE COMUNICACAO UTILIZADA PELA ROTINA DCOM/RURA        *
      *    TABELA RURCV010 - DB2PRD.V01FINRURA_PRORURA (FINALIDADES)  *
      *    TAMANHO : 528                                              *
      *****************************************************************
       01  AREA-RURC10.
           05 AREA-ENT-RURC10.
              10 TIPO-RETORNO-RURC10           PIC X(01).
              10 RESTART-RURC10                PIC X(01).
              10 QTDE-RESTART-RURC10           PIC 9(07).
              10 CFNALD-RURAL-RURC10           PIC 9(03).
              10 CPRODT-RURAL-RURC10           PIC 9(03).
           05 AREA-SAI-RURC10.
              10 AREA-SAI-COD-RETORNOS-RURC10.
                 15 COD-RETORNO-RURC10         PIC 9(02).
                 15 POS-REF-ERRO-RURC10        PIC X(04).
                 15 TABELA-ERRO-RURC10         PIC X(18).
                 15 COMANDO-ERRO-RURC10        PIC X(10).
                 15 QTD-OCORR-RURC10           PIC 9(03).
                 15 SQLCODE-RURC10             PIC S9(09) COMP.
                 15 SQLERRM-RURC10.
                    20 SQLERRML-RURC10         PIC S9(04) COMP.
                    20 SQLERRMC-RURC10         PIC X(70).
              10 AREA-RETORNO-RURC10 OCCURS 50.
                 15 RT-CFNALD-RURAL-RURC10     PIC S9(3)V USAGE COMP-3.
                 15 RT-CPRODT-RURAL-RURC10     PIC S9(3)V USAGE COMP-3.
                 15 RT-CUND-MEDD-FINCD-RURC10  PIC S9(3)V USAGE COMP-3.
                 15 RT-CUND-MEDD-PROVV-RURC10  PIC S9(3)V USAGE COMP-3.
