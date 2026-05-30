      *---------------------------------------------------------------*
      *               C A P G E M I N I                               *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#DCOMXR - PARA MODULO RUEC8850                 *
      * GERACAO.....: JULHO/2020                                      *
      *---------------------------------------------------------------*
      * OBJETIVO....: INCLUSAO DOS DADOS RETORNADOS PELA COR0007R1    *
      *               NA TABELA RUECB046                              *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
      **
       01  DCOMXR-AREA.
           05  DCOMXR-ENVIO.
             10  DCOMXR-PGM-CHAMADOR         PIC  X(008).
             10  DCOMXR-NRO-CTRLIF           PIC  X(020).
             10  DCOMXR-DANO-OPER-DESC       PIC  9(004).
             10  DCOMXR-NSEQ-OPER-DESC       PIC  9(009).

      **
      *************** AREA DE DADOS DE PAGINACAO ***************
      **
           05 DCOMXR-PAGINACAO.

             10  DCOMXR-COD-REF-PAG-I        PIC  9(005).
             10  DCOMXR-NUM-REF-BCCOR-I      PIC  X(011).

             10  DCOMXR-COD-REF-PAG-F        PIC  9(005).
             10  DCOMXR-NUM-REF-BCCOR-F      PIC  X(011).

             10  DCOMXR-IND-PAGINA           PIC  X(001) VALUE  SPACES.
                 88 DCOMXR-P-INICIAL         VALUE 'I'.
                 88 DCOMXR-P-PRIMEIRA        VALUE 'P'.
                 88 DCOMXR-P-SEGUINTE        VALUE 'S'.
                 88 DCOMXR-P-ANTERIOR        VALUE 'A'.
                 88 DCOMXR-P-ULTIMA          VALUE 'U'.


      **
      *************** AREA DE DADOS DE RETORNO ***************
      **
           05  DCOMXR-RETORNO.
             10  DCOMXR-COD-RET              PIC  9(005).
      **
      **=======  00 - OK
      **=======  01 - DADOS INCONSISTENTES
      **=======  03 - NAO ENCONTRADO
      **=======  99 - ERRO DB2
             10  DCOMXR-MENSAGEM             PIC  X(079).
             10  FILLER                      PIC  X(050).
             10  DCOMXR-SQLCA                PIC  X(136).
             10  DCOMXR-ERRO-AREA            PIC  X(107).
             10  DCOMXR-TP-BENEF             PIC  X(001).
             10  DCOMXR-CNPJ-CPF-BENEF       PIC  9(015).
             10  DCOMXR-DT-INI-PESQ          PIC  X(010).
             10  DCOMXR-DT-FIM-PESQ          PIC  X(010).
             10  DCOMXR-NRO-CTRL-IF-S        PIC  X(020).
             10  DCOMXR-NUM-CONSULTAS        PIC  9(005).
             10  DCOMXR-REG-OCORRENCIA       OCCURS 9 TIMES.
                 15  DCOMXR-COD-REF-PAG               PIC 9(005).
                 15  DCOMXR-NUM-REF-BCCOR             PIC X(011).
                 15  DCOMXR-ANO-MES-REF               PIC X(007).
                 15  DCOMXR-SIT-OPCOR                 PIC X(034).
