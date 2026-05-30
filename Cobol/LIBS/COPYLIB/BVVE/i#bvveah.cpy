      *---------------------------------------------------------------*
      *     MOVIMENTO DE RESUMO DE FATURAMENTO         30/04/1998     *
      *                                                               *
      *     LRECL= 60           INC= I#BVVEAH          ENTRADA        *
      *---------------------------------------------------------------*

       01  REG-HEADER.
           10  HDR-TIPO-REGISTRO     PIC X(01).
      ****    FORMATO DO CAMPO HDR-PERIODO   AAAAMM                ****
           10  HDR-PERIODO.
               15  HDR-PERIODO-ANO   PIC 9(04).
               15  HDR-PERIODO-MES   PIC 9(02).
           10  FILLER                PIC X(53).



       01  REG-DETALHE.
           10  FILLER                PIC X(01).
           10  DTL-AGENCIA           PIC 9(04).
           10  DTL-CONTA             PIC 9(07).
           10  DTL-DG-CONTA          PIC X(01).
           10  DTL-NOME-CONTA        PIC X(30).
           10  DTL-VLR-CREDITO       PIC 9(11)V9(02).
           10  FILLER                PIC X(04).



       01  REG-TRAILLER.
           10  FILLER                PIC X(01).
           10  TRL-QTDE-REGS         PIC 9(06).
           10  TRL-TOTAL-CREDITO     PIC 9(11)V9(02).
           10  FILLER                PIC X(40).
