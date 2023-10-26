SET DATEFIRST 7
SET ANSI_NULLS OFF
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
SET LOCK_TIMEOUT -1
SET QUOTED_IDENTIFIER OFF
GO

IF EXISTS(SELECT * FROM sysobjects WHERE TYPE='p' AND NAME='xpFBHAplicarBecaEnferm')
DROP PROCEDURE xpFBHAplicarBecaEnferm
GO
CREATE PROCEDURE xpFBHAplicarBecaEnferm
@ID		  INT,
@VentaID	  INT
AS
BEGIN
	DECLARE @PlanEstudio		VARCHAR(25),
		  @TipoBeca			VARCHAR(50),
		  @CicloEscolar		VARCHAR(15),
		  @Alumno				VARCHAR(20),
		  @DescBeca			FLOAT,
		  @Empresa			VARCHAR(10)
		  
	SELECT @PlanEstudio=c.PlanEstudios
		  ,@CicloEscolar=c.CicloEscolar
		  ,@Alumno=c.Alumno
		  ,@Empresa=c.Empresa
    FROM CE c
	WHERE ID=@ID  
	
	SELECT @TipoBeca=d.Beca
		FROM CE c
		JOIN CED d ON c.ID=d.ID
		WHERE c.Mov='Beca'
		AND c.Estatus='CONCLUIDO'
		AND c.Alumno=@Alumno
		AND c.CicloEscolar=@CicloEscolar
		AND c.PlanEstudios=@PlanEstudio
		
    IF @TipoBeca IS NOT NULL
    BEGIN
    	   SELECT @DescBeca=cc.PorcdeDesc
    	   FROM CEBecaCiclo AS cc
    	   WHERE cc.Ciclo=@CicloEscolar
    	   AND cc.Beca=@TipoBeca
    	   
    	   
    END
		  
RETURN
END